.onLoad <- function(libname, pkgname) { # nolint .onLoad cannot be snake case

  suppressPackageStartupMessages(
    lapply(
      c("beautier", "beastier", "tracerer", "mauricer", "babette", "mcbette"),
      library,
      character.only = TRUE,
      warn.conflicts = FALSE
    )
  )
  invisible()
}
