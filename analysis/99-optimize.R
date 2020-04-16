if (Sys.info()[["user"]] == "seananderson") {
  files_per_core <- 2
  setwd("figs-ms")
  system(paste0(
    "find -X . -name '*.png' -print0 | xargs -0 -n ",
    files_per_core, " -P ", parallel::detectCores() / 2, " optipng -strip all"
  ))
  setwd("..")
}
