# # # No documentation block yet, just the methods
# # print.spax <- function(x, ...) {
# #   cat("spax object\n")
# #   invisible(x)
# # }
# #
# # summary.spax <- function(object, ...) {
# #   cat("Summary of spax object\n")
# #   invisible(object)
# # }
#
# # Register methods manually in .onLoad
# .onLoad <- function(libname, pkgname) {
#   registerS3method("print", "spax", print.spax, envir = asNamespace(pkgname))
#   #registerS3method("summary", "spax", summary.spax, envir = asNamespace(pkgname))
# }
