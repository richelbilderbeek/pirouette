#' Check if the function to create a true alignment with (from the
#' true phylogeny) is valid.
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{check_sim_twal_fun} to check a function to
#' generate a twin alignment
#' @export
check_sim_tral_fun <- function(sim_tral_fun) {
  if (!is.function(sim_tral_fun)) {
    stop("'sim_tral_fun' must be a function")
  }
  # sim_tral_fun must return a DNAbin
  out <- NA
  tryCatch({
      out <- sim_tral_fun(
        true_phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
        root_sequence = "acgt"
      )
    }, error = function(e) {
      stop(
        "Error when using 'sim_tral_fun' on an example ",
        "phylogeny. \n",
        "Error message: ", e$message
      )
    }
  )
  if (!inherits(out, "DNAbin")) {
    stop(
      "'sim_tral_fun' must be a function that returns",
      " an ape::DNAbin. \n",
      "Actual class returned: ", class(out)
    )
  }
  invisible(sim_tral_fun)
}
