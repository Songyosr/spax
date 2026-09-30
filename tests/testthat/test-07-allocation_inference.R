.inference_fixture <- function(colocated = FALSE) {
  n <- 30L; m <- 24L
  d <- sqrt(outer(seq(1,35,length.out=n),seq(1,35,length.out=m),"-")^2 +
    outer(4*sin(seq_len(n)),3*cos(seq_len(m)),"-")^2)+1
  if(colocated) d <- matrix(rep(seq(1,35,length.out=n),m),n,m)
  dimnames(d) <- list(paste0("o",seq_len(n)),paste0("f",seq_len(m)))
  D <- setNames(rep(200,n),rownames(d)); S <- setNames(rep(c(4,7,5),length.out=m),colnames(d))
  model <- clm_allocation(prepare_allocation(D,S,d),fit_v0=TRUE)
  truth <- c(sigma=8,v0=25); eq <- evaluate_allocation(model,truth)
  y <- round(eq$outputs$allocation*D)
  dimnames(y) <- dimnames(d)
  starts <- rbind(a=c(sigma=7,v0=20),b=c(sigma=12,v0=50))
  list(model=model,D=D,S=S,d=d,truth=truth,y=y,starts=starts,
    lower=c(sigma=2,v0=1),upper=c(sigma=40,v0=200))
}
.inference_fit <- function(f,y,output="utilization") suppressWarnings(fit_allocation(
  f$model,y,f$starts,f$lower,f$upper,output=output,loss="poisson",gradient=TRUE,
  control=list(maxit=200,factr=1e4,pgtol=1e-7)))
.inference_observation <- function(f,y,output="utilization",sampling="independent_poisson",groups=NULL) {
  .allocation_observation(f$model,y,output,event="events",exposure_units="opportunities",
    period="year",exposure="poisson_mean",sampling=sampling,groups=groups)
}
test_that("inference aligns masked counts and dependence declarations by IDs", {
  f <- .inference_fixture(); y <- f$y; y[2,3] <- NA
  y[,24] <- NA; y[1,1] <- 0
  fit <- .inference_fit(f,y,"flow")
  groups <- setNames(colnames(y),colnames(y))
  a <- .inference_observation(f,y,"flow","independent_groups",groups)
  b <- .inference_observation(f,y[30:1,24:1],"flow","independent_groups",rev(groups))
  ia <- .allocation_inference(f$model,fit,a,group_adjustment="none")
  ib <- .allocation_inference(f$model,fit,b,group_adjustment="none")
  expect_identical(ia$covariance_log,ib$covariance_log)
  expect_identical(ia$fitted_mean,ib$fitted_mean)
  tampered <- a; tampered$target$values <- rev(tampered$target$values)
  expect_error(.allocation_inference(f$model,fit,tampered),"contract conflicts")
  tampered <- a; tampered$target$mask <- rev(tampered$target$mask)
  expect_error(.allocation_inference(f$model,fit,tampered),"contract conflicts")
  expect_equal(ia$diagnostics$independent_groups,23L)
  expect_equal(sum(ia$diagnostics$group_leverage),ia$diagnostics$rank,tolerance=1e-8)
  J <- ia$jacobian_log; mu <- ia$fitted_mean; obs <- a$target$values
  score <- J*((obs-mu)/mu); meat <- crossprod(rowsum(score,a$labels[a$target$mask],reorder=FALSE))
  B <- crossprod(J/sqrt(mu)); V <- solve(B)%*%meat%*%solve(B)*23/22
  expect_equal(unname(ia$covariance_log),unname(V),tolerance=1e-7)
  hc3 <- .allocation_inference(f$model,fit,a)
  W <- J/sqrt(mu); residual <- (obs-mu)/sqrt(mu)
  labels <- a$labels[a$target$mask]
  adjusted <- t(vapply(unique(labels),function(label) {
    sel <- labels==label; block <- W[sel,,drop=FALSE]
    as.numeric(crossprod(block,solve(diag(sum(sel))-block%*%solve(B)%*%t(block),residual[sel])))
  },numeric(ncol(W))))
  independent <- solve(B)%*%crossprod(adjusted)%*%solve(B)
  expect_equal(unname(hc3$covariance_log),unname(independent),tolerance=1e-7)
  expect_identical(hc3$diagnostics$group_adjustment,"HC3")
  expect_true(hc3$valid_regular_interval)
  expect_false(ia$valid_regular_interval)
  expect_true("uncorrected_group_comparator" %in% ia$reasons)
  stale <- ia; stale$valid_regular_interval <- TRUE; stale$reasons <- character()
  stale_q <- .allocation_estimands(stale,function(theta,z) c(sigma=unname(theta[1])))$table
  expect_identical(stale_q$status,"uncorrected_group_comparator")
  expect_true(is.na(stale_q$lower))
  expect_lt(max(hc3$diagnostics$group_max_leverage),1)
  expect_null(ia$base$outputs$allocation); expect_null(ia$perturbations[[1]]$plus$outputs$flow)
  expect_equal(predict_allocation(ia$base,f$d,outputs="rho")$outputs$rho,
    predict_allocation(fit,f$d,outputs="rho")$outputs$rho)
  expect_error(.inference_observation(f,colSums(f$y)+.2),"whole counts")
  expect_error(.inference_observation(f,colSums(f$y),sampling="independent_groups",groups=unname(groups)),"facility ID")
  changed <- f$model; changed$substrate$D_active[1] <- changed$substrate$D_active[1]+1
  expect_error(.allocation_inference(changed,fit,a),"contract conflicts")
  changed <- f$model; changed$substrate$distance_active[1,1] <- changed$substrate$distance_active[1,1]+1
  expect_error(.allocation_inference(changed,fit,a),"conflicts")
  modified <- f$model; modified$substrate$input_metadata$travel_units <- "seconds"
  declared <- .inference_observation(list(model=modified),y,"flow","independent_groups",groups)
  expect_error(.allocation_inference(modified,fit,declared),"source conflicts")
})
test_that("source agreement includes supply invisible to calibration support", {
  f <- .inference_fixture(); d <- f$d; d[,24] <- Inf
  f$model <- clm_allocation(prepare_allocation(f$D,f$S,d),fit_v0=TRUE)
  y <- round(evaluate_allocation(f$model,f$truth)$utilization)
  fit <- .inference_fit(f,y)
  S1 <- f$S; S1[24] <- S1[24]*2
  changed <- clm_allocation(prepare_allocation(f$D,S1,d),fit_v0=TRUE)
  ob <- .inference_observation(list(model=changed),y)
  expect_error(.allocation_inference(changed,fit,ob),"source conflicts")
})
test_that("underflow is not structural absence and a live floor suppresses intervals", {
  f <- .inference_fixture(); d <- f$d; d[1,] <- 1e6
  f$model <- clm_allocation(prepare_allocation(f$D,f$S,d),fit_v0=TRUE)
  y <- round(evaluate_allocation(f$model,f$truth)$outputs$allocation*f$D); dimnames(y) <- dimnames(d)
  fit <- .inference_fit(f,y,"flow")
  info <- .allocation_inference(f$model,fit,.inference_observation(f,y,"flow"))
  expect_true("nonregular_mean_or_loss_floor"%in%info$reasons)
  expect_false(info$valid_regular_interval)
  f <- .inference_fixture(); y <- colSums(f$y); fit <- .inference_fit(f,y)
  fit$best$loss_args <- list(eps=1e9)
  fit$best$loss <- .poisson_loss(fit$best$predicted,y,eps=1e9)
  info <- .allocation_inference(f$model,fit,.inference_observation(f,y))
  expect_true("nonregular_mean_or_loss_floor"%in%info$reasons)
})
test_that("co-location preserves aggregate support while distinguishing totals and flows", {
  f <- .inference_fixture(TRUE); y <- colSums(f$y)
  fit <- .inference_fit(f,y); info <- .allocation_inference(f$model,fit,.inference_observation(f,y))
  callback <- function(theta,z) c(sigma=unname(theta[1]),v0=unname(theta[2]),rho=sum(z$utilization)/sum(f$D),A=sum(f$S)/sum(f$D))
  q <- .allocation_estimands(info,callback,c("log","log","logit","identity"),c(FALSE,FALSE,FALSE,TRUE))
  expect_equal(info$diagnostics$rank,1)
  expect_identical(q$table$status[1:2],rep("unsupported_local_direction",2))
  expect_lt(q$table$null_fraction[3],1e-4)
  expect_identical(q$table$status[4],"fixed_input_identity")
  groups <- setNames(names(f$S),names(f$S))
  grouped <- .allocation_inference(f$model,fit,.inference_observation(f,y,sampling="independent_groups",groups=groups))
  expect_equal(grouped$diagnostics$rank,1)
  expect_true(all(is.finite(grouped$covariance_log)))
  expect_false(grouped$valid_regular_interval)
  expect_true("grouped_totals_not_validated" %in% grouped$reasons)
  expect_identical(.allocation_estimands(grouped,callback,c("log","log","logit","identity"),
    c(FALSE,FALSE,FALSE,TRUE))$table$status[1:2],rep("unsupported_local_direction",2))
  flow_fit <- .inference_fit(f,f$y,"flow")
  flow_info <- .allocation_inference(f$model,flow_fit,.inference_observation(f,f$y,"flow"))
  expect_equal(flow_info$diagnostics$rank,2)
  expect_error(.allocation_estimands(info,function(theta,z) c(a=1,a=2)),"unique named")
  wrong <- q$gradient_log[4:1,,drop=FALSE]
  expect_error(.allocation_delta(info,setNames(q$table$estimate,q$table$quantity),wrong),"quantity and parameter order")
})
test_that("a dominant group cannot produce regular HC3 intervals", {
  f <- .inference_fixture(); S <- f$S; S[6:24] <- 1e-9
  f$model <- clm_allocation(prepare_allocation(f$D,S,f$d),fit_v0=TRUE)
  y <- round(evaluate_allocation(f$model,f$truth)$utilization)
  fit <- .inference_fit(f,y)
  groups <- setNames(c(rep("dominant",5),paste0("g",6:24)),names(S))
  info <- .allocation_inference(f$model,fit,.inference_observation(f,y,sampling="independent_groups",groups=groups))
  expect_equal(info$diagnostics$independent_groups,20)
  expect_true("dominant_group_leverage"%in%info$reasons)
  expect_false(info$valid_regular_interval)
  expect_true(any(info$diagnostics$group_max_leverage>=1-1e-8))
})
test_that("paired scenario inference uses the difference gradient and one covariance", {
  f <- .inference_fixture(); y <- colSums(f$y); fit <- .inference_fit(f,y)
  info <- .allocation_inference(f$model,fit,.inference_observation(f,y))
  S1 <- f$S; S1[2] <- S1[2]*1.2
  changed <- clm_allocation(prepare_allocation(f$D,S1,f$d),fit_v0=TRUE)
  callback <- function(theta,z) {
    baseline <- sum(z$utilization); scenario <- sum(evaluate_allocation(changed,theta)$utilization)
    c(baseline=baseline,scenario=scenario,difference=scenario-baseline)
  }
  q <- .allocation_estimands(info,callback)
  expect_equal(q$gradient_log[3,],q$gradient_log[2,]-q$gradient_log[1,],tolerance=1e-8)
  g <- q$gradient_log[3,]
  expect_equal(q$table$standard_error[3]^2,as.numeric(crossprod(g,info$covariance_log%*%g)),tolerance=1e-8)
  groups <- setNames(rep(paste0("g",1:8),each=3),names(f$S))
  few <- .allocation_inference(f$model,fit,.inference_observation(f,y,sampling="independent_groups",groups=groups))
  expect_false(few$valid_regular_interval); expect_equal(few$diagnostics$independent_groups,8)
  expect_true(all(grepl("few_independent_groups",.allocation_estimands(few,callback)$table$status)))
  one <- setNames(rep("one",length(f$S)),names(f$S))
  one_info <- .allocation_inference(f$model,fit,.inference_observation(f,y,sampling="independent_groups",groups=one))
  expect_no_warning(one_q <- .allocation_estimands(one_info,callback))
  expect_true(all(grepl("few_independent_groups",one_q$table$status)))
  bounded <- fit; bounded$boundary[,] <- TRUE
  bi <- .allocation_inference(f$model,bounded,.inference_observation(f,y))
  expect_true("parameter_boundary"%in%bi$reasons)
  floored <- fit; floored$best$loss_args <- list(eps=1e9)
  expect_error(.allocation_inference(f$model,floored,.inference_observation(f,y)),"loss conflict")
  unnamed <- fit; unnamed$best$loss_args <- list(1e9)
  expect_error(.allocation_inference(f$model,unnamed,.inference_observation(f,y)),"unweighted Poisson fit")
})
test_that("grouped total covariances remain diagnostics without regular intervals", {
  f <- .inference_fixture(); y <- colSums(f$y); fit <- .inference_fit(f,y)
  callback <- function(theta,z) c(sigma=unname(theta[1]),rho=sum(z$utilization)/sum(f$D))
  groups <- setNames(names(f$S),names(f$S))
  for (adjustment in c("HC3","none")) {
    info <- .allocation_inference(f$model,fit,
      .inference_observation(f,y,sampling="independent_groups",groups=groups),group_adjustment=adjustment)
    expect_equal(info$diagnostics$rank,2)
    expect_true(all(is.finite(info$covariance_log)))
    expect_false(info$valid_regular_interval)
    q <- .allocation_estimands(info,callback,c("log","logit"))$table
    expect_true(all(grepl("grouped_totals_not_validated",q$status)))
    expect_true(all(is.na(q$standard_error) & is.na(q$lower) & is.na(q$upper)))
    stale <- info; stale$valid_regular_interval <- TRUE; stale$reasons <- character()
    restored <- unserialize(serialize(stale,NULL))
    q <- .allocation_estimands(restored,callback,c("log","logit"))$table
    expect_true(all(grepl("grouped_totals_not_validated",q$status)))
    expect_true(all(is.na(q$standard_error) & is.na(q$lower) & is.na(q$upper)))
  }
})
