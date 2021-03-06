#' Convert a tree into branching times
#'
#' Convert a tree into branching times.
#' Differently from the \link[ape]{branching.times} function in \link{ape},
#' it will keep the multiple events. Since the units are million
#' years, a precision of 8 means that the approximation goes up to the 8-th
#' digits. With such approximation we consider events happening within an
#' interval of 4 days (1 million years / 10^8 = 1 year / 100) as simultaneous.
#' @inheritParams default_params_doc
#' @return the branching times
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
#'
#' # c(3.0, 2.0)
#' convert_tree2brts(phylogeny)
#' @export
convert_tree2brts <- function(tree, precision = 8) {
  round(ape::branching.times(tree), digits = precision)
}

#' @title Twin models
#' @description Twin models
#' @return the twin models
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' get_twin_models()
#' @export
get_twin_models <- function() {
  c("birth_death", "yule", "copy_true")
}

#' @title Twin methods
#' @description Twin methods
#' @return the twin methods
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' get_twin_methods()
#' @export
get_twin_methods <- function() {
  c(
    "random_tree",
    "max_clade_cred",
    "max_likelihood"
  )
}

#' Get the names of the model types
#' @return the model types
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' get_model_type_names()
#' @export
get_model_type_names <- function() {
  c("generative", "candidate")
}

#' Get the names of the tree types
#' @return the tree types
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' get_tree_types()
#' @export
get_tree_types <- function() {
  c("true", "twin")
}
