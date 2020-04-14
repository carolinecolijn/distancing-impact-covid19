if (!identical(.Platform$OS.type, "windows")) {
  files_per_core <- 4
  setwd("figs-ms")
  system(paste0(
    "find -X . -name '*.png' -print0 | xargs -0 -n ",
    files_per_core, " -P ", parallel::detectCores() / 2, " optipng -strip all"
  ))
  setwd("..")
}
