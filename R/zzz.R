# .onLoad <- function(libname, pkgname) {
#   withr::defer({
#     if (exists("ojo_pool", envir = ojo_env(), inherits = FALSE)) {
#       pool::poolClose(ojo_env()$ojo_pool)
#       rm("ojo_pool", envir = ojo_env())
#     }
#   })
#
#   # Fun message
#   if(Sys.getenv("OJO_LOAD_MESSAGE") != FALSE){
#   ojo_art <- r"{                   _           _ _
#  ╔══    ══╗   ___ (_) ___   __| | |__
#  ║/      \║  / _ \| |/ _ \ / _` | '_ \
#             | (_) | | (_) | (_| | |_) |
#  ║\      /║  \___// |\___/ \__,_|_.__/
#  ╚══    ══╝     |__/
#                           version }"
#
#   emoji_list <- c("😀", "😃", "😄", "😁", "😆", "😊", "😎", "😜", "😝", "😛", "🤠", "🙂", "🤔")
#   random_emoji <- sample(emoji_list, 1)
#   ojo_art_full <- paste0(ojo_art, as.character(ojo_version()), " ", random_emoji)
#   message(cat(ojo_art_full, sep = "\n"))
# }
#
# }

ojodbStartupMessage <- function() {

  ojo_art <- r"{                   _           _ _
 ╔══    ══╗   ___ (_) ___   __| | |__
 ║/      \║  / _ \| |/ _ \ / _` | '_ \
            | (_) | | (_) | (_| | |_) |
 ║\      /║  \___// |\___/ \__,_|_.__/
 ╚══    ══╝     |__/
                          version }"

  emoji_list <- c("😀", "😃", "😄", "😁", "😆", "😊", "😎", "😜", "😝", "😛", "🤠", "🙂", "🤔")
  random_emoji <- sample(emoji_list, 1)
  ojo_art_full <- paste0(ojo_art, as.character(ojo_version()), " ", random_emoji)
  startup_message <- paste0(ojo_art_full, sep = "\n")

  return(startup_message)

}

.onAttach <- function(lib, pkg) {

  if(Sys.getenv("OJO_LOAD_MESSAGE") != FALSE){
    msg <- ojodbStartupMessage()
    if(!interactive())
      msg[1] <- paste("Package 'ojodb' version", utils::packageVersion("ojodb"))
    packageStartupMessage(msg)
    invisible()
  }

}
