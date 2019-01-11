#' @title Convert a tree into branching times
#' @description Convert a tree into branching times. Differently from the ape's
#'  function, it will keep the multiple events. Since the units are million
#'  years, a precision of 8 means that the approximation goes up to the 8-th
#'  digits. With such approximation we consider events happening within an
#'  interval of 4 days (1 million years / 10^8 = 1 year / 100) as simultaneous.
#' @inheritParams default_params_doc
#' @return the branching times
#' @author Giovanni Laudanno
convert_tree2brts <- function(tree, precision = 8) {

  brts0 <- ape::branching.times(tree)
  brts <- DDD::roundn(brts0, digits = precision)

  brts
}

#' @title Site models
#' @description Site models
#' @inheritParams default_params_doc
#' @return the site models
#' @author Giovanni Laudanno
get_site_models <- function() {
  beautier::get_site_model_names()
}

#' @title Clock models
#' @description Clock models
#' @inheritParams default_params_doc
#' @return the clock models
#' @author Giovanni Laudanno
get_clock_models <- function() {
  beautier::get_clock_model_names()
}

#' @title Convert bd phylo to L table
#' @description Convert bd phylo to L table. Don't use for mbd.
#' @inheritParams default_params_doc
#' @return the L table
#' @author Xu Liang, Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
bd_phylo_2_l_table <- function(
  phylo
) {
  l_table <- dododo::phylo2L(phylo) # nolint
  colnames(l_table) <- c("birth_time", "parent", "id", "death_time")
  return(l_table)
}
