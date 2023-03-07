.onLoad <- function(libname, pkgname) {
  withr::defer({
    if (exists("ojo_pool", envir = ojo_env(), inherits = FALSE)) {
      pool::poolClose(ojo_env()$ojo_pool)
      rm("ojo_pool", envir = ojo_env())
    }
  })
}