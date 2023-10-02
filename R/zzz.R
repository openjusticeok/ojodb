.onLoad <- function(lib, pkg) {
  S7::methods_register()
}

ojodbStartupMessage <- function() {

  ojo_art <- "                   _           _ _\n \u2554\u2550\u2550    \u2550\u2550\u2557   ___ (_) ___   __| | |__\n \u2551/      \\\u2551  / _ \\| |/ _ \\ / _` | '_ \\\n            | (_) | | (_) | (_| | |_) |\n \u2551\\      /\u2551  \\___// |\\___/ \\__,_|_.__/\n \u255a\u2550\u2550    \u2550\u2550\u255d     |__/\n                          version "

  emoji_list <- c("\U0001F600", "\U0001F603", "\U0001F604", "\U0001F601", "\U0001F606",
                  "\U0001F60A", "\U0001F60E", "\U0001F61C", "\U0001F61D", "\U0001F61B",
                  "\U0001F920", "\U0001F642", "\U0001F914")
  random_emoji <- sample(emoji_list, 1)
  ojo_art_full <- paste0(ojo_art, as.character(ojo_version()), " ", random_emoji)
  startup_message <- paste0(ojo_art_full, sep = "\n")

  return(startup_message)

}

.onAttach <- function(lib, pkg) {

  if (Sys.getenv("OJO_LOAD_MESSAGE") != FALSE) {
    msg <- ojodbStartupMessage()
    if (!interactive())
      msg[1] <- paste("Package 'ojodb' version", utils::packageVersion("ojodb"))
    packageStartupMessage(msg)
    invisible()
  }

}
