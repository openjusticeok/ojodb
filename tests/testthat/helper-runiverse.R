# Shoutout to https://github.com/mhpob/matos/
skip_on_runiverse <- function() {
  skip_if(
    Sys.getenv("MY_UNIVERSE", "") != "",
    "On R-universe."
  )
}
