.onLoad <- function(libname, pkgname) {
    register_s3_method("vctrs", generic = "vec_restore", class = "sol_property", fun = vec_restore.sol_property)
}

register_s3_method <- function(pkg, generic, class, fun) {
    if (pkg %in% loadedNamespaces()) registerS3method(generic, class, fun, envir = asNamespace(pkg))
    setHook(packageEvent(pkg, "onLoad"), function(...) registerS3method(generic, class, fun, envir = asNamespace(pkg)))
}
