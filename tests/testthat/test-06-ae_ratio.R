.ratio_fixture <- function() {
  d <- terra::rast(nrows = 2, ncols = 3)
  terra::values(d) <- c(100, 80, 40, 20, 0, NA)
  dist <- c(d,d,d)
  names(dist) <- c("a","b","c")
  terra::values(dist) <- cbind(c(5,10,30,NA,2,5), c(25,10,5,NA,3,6), rep(NA_real_,6))
  h <- .huff_problem(d, c(a=120,b=90,c=30), dist, fit_v0=TRUE)
  list(static=h, ratio=.ratio_problem(h$substrate))
}

test_that("ratio null is exact CLM and retains disconnected support", {
  f <- .ratio_fixture()
  t <- c(sigma=20,v0=30,feedback_scale=1)
  a <- .solve_problem(f$static,t[1:2])$outputs
  b <- .solve_problem(f$ratio,t)$outputs
  expect_equal(b$allocation,a$allocation,tolerance=1e-14)
  expect_equal(b$utilization,a$utilization,tolerance=1e-12)
  expect_equal(b$access+b$outside_share,rep(1,4))
  expect_equal(b$access[4],0)
  expect_equal(b$utilization[3],0)
  expect_equal(unname(.problem_jac_state(f$ratio,rep(0,3),t)$jac_state),matrix(0,3,3))
  cov <- .problem_coverage(f$ratio,t)
  expect_true(cov$conservation$passed)
  expect_equal(cov$conservation$excluded_facilities,1)
  feedback <- c(sigma=20,v0=30,feedback_scale=1.4)
  expect_true(.problem_coverage(f$ratio,feedback,
    solve_control=list(lambda=.4,tol=1e-10))$conservation$passed)
  expect_error(.problem_coverage(f$ratio,feedback,
    solve_control=list(max_iter=1,warn=FALSE)),"converged solve")
  bad <- f$static$substrate; bad$S[1] <- 0
  expect_error(.ratio_problem(bad),"positive measured supply")
  expect_error(.bind_theta(f$ratio,c(sigma=20,v0=30,feedback_scale=.9)),"bounds")
})

test_that("ratio coefficients preserve legacy and reference-scale mappings", {
  f <- .ratio_fixture()
  p <- .ratio_problem(f$static$substrate,b=0)
  t <- c(sigma=20,v0=30,feedback_scale=1.8)
  U <- c(30,50,0)
  K <- exp(-f$static$substrate$distance_active^2/(2*20^2)); K[is.na(K)] <- 0
  phi <- sweep(K,2,(as.numeric(p$substrate$S)/(U+1e-6))^.8,`*`)
  expect_equal(.problem_outputs(p,U,t)$allocation,phi/(rowSums(phi)+30))
  scaled <- .ratio_problem(p$substrate,b=0,ratio_ref=3)
  ts <- t; ts['v0'] <- t['v0']/3^.8
  expect_equal(.problem_outputs(scaled,U,ts)$allocation,.problem_outputs(p,U,t)$allocation)
})

test_that("analytic derivatives and profile nuisance columns match differences", {
  f <- .ratio_fixture()
  p <- .ratio_problem(f$static$substrate,fit_b=TRUE)
  t <- c(sigma=20,v0=30,feedback_scale=1.4,supply_scale=2.2)
  eq <- .solve_problem(p,t,lambda=.4,tol=1e-11,max_iter=1000)
  expect_true(eq$converged)
  expect_lt(max(abs(.problem_map(p,eq$x_star,t)-eq$x_star)),1e-9)
  # Interior state for central differences; unreachable facility remains explicit.
  x <- pmax(eq$x_star,1)
  step <- .bind_theta(p,t)
  expect_equal(step$jac_state(x)$jac_state,fd_jacobian_state(step$map,x,check=FALSE),tolerance=1e-6,ignore_attr=TRUE)
  expect_equal(step$jac_param(x),fd_jacobian(function(z) .problem_map(p,x,z),t,check=FALSE),
               tolerance=1e-6,ignore_attr=TRUE)
  sensitivity <- .problem_output_sensitivity(p,t,eq$x_star)
  oracle <- fd_jacobian(function(z) .solve_problem(p,z,lambda=.4,tol=1e-11,max_iter=1000,
                         diagnostics=FALSE)$utilization,t,check=FALSE)
  expect_equal(sensitivity,oracle,tolerance=2e-5,ignore_attr=TRUE)
  fixed <- .fix_problem_parameter(p,"feedback_scale",1.4)
  nuisance <- t[names(t)!="feedback_scale"]
  expect_equal(.problem_output_sensitivity(fixed,nuisance,eq$x_star),
               sensitivity[,names(nuisance),drop=FALSE],tolerance=1e-10,ignore_attr=TRUE)
  # NULL hook preserves the finite-difference path for edge-valued outputs.
  jac <- .problem_output_jac_param(p,t,x,"flow")
  expect_equal(dim(jac),c(12L,4L))
})

test_that("shared fitter profiles and recovers generated feedback without re-preparation", {
  f <- .ratio_fixture()
  truth <- c(sigma=20,v0=30,feedback_scale=1.5)
  y <- .solve_problem(f$ratio,truth,lambda=.4,tol=1e-11)$outputs$flow
  y[4,] <- NA
  # Rich flow target supplies destination patterns as well as totals.
  fit <- .fit_problem_nfxp(f$ratio,y,init=c(sigma=18,v0=25,feedback_scale=1.3),
    lower=c(sigma=5,v0=1,feedback_scale=1),upper=c(sigma=50,v0=100,feedback_scale=2),
    output="flow",gradient=TRUE,lambda=.4,tol=1e-10,control=list(maxit=100))
  expect_equal(fit$convergence,0)
  expect_true(fit$equilibrium$converged)
  expect_lt(fit$loss,1e-7)
  expect_equal(fit$theta,truth,tolerance=2e-3)
  cov <- .ae_coverage(fit)
  expect_true(cov$conservation$passed)
  static <- .fix_problem_parameter(f$ratio,"feedback_scale",1)
  null <- .fit_problem_nfxp(static,y,c(sigma=20,v0=30),c(sigma=5,v0=1),c(sigma=50,v0=100),
    output="flow",gradient=TRUE,lambda=.4,tol=1e-10,control=list(maxit=100))
  expect_gt(null$loss,fit$loss+1e-3)
})
