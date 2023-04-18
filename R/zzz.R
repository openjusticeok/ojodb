.onLoad <- function(libname, pkgname) {
  withr::defer({
    if (exists("ojo_pool", envir = ojo_env(), inherits = FALSE)) {
      pool::poolClose(ojo_env()$ojo_pool)
      rm("ojo_pool", envir = ojo_env())
    }
  })

  # Fun message
  ojo_art <- r"{
                   _           _ _
 ╔══    ══╗   ___ (_) ___   __| | |__
 ║/      \║  / _ \| |/ _ \ / _` | '_ \
            | (_) | | (_) | (_| | |_) |
 ║\      /║  \___// |\___/ \__,_|_.__/
 ╚══    ══╝     |__/
                          version }"

  ojo_art_full <- paste0(ojo_art, as.character(ojo_version()))
  message(cat(ojo_art_full, sep = "\n"))

}
