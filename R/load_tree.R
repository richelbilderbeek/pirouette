#' Load pre-simulated tree generated under requested model and seed
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @export
load_tree <- function(tree_model = "mbd", seed = 1) {
  filename <- system.file(
    file.path(
      "extdata",
      "models",
      tree_model
    ),
    paste0("tree_", seed),
    package = "pirouette"
  )
  if (!file.exists(filename)) {
    stop("This file does not exist! Try with different model name and/or seed.")
  }
  tree <- ape::read.tree(file = filename)
  tree
}
