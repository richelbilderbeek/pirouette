#' Create a testing phylogeny with 3 taxa and a crown age of 3
#'
#' @return a \code{phylo} from the \code{ape} package
#' @author Richèl J.C. Bilderbeek
#' @export
create_test_phylogeny <- function() {
  ape::read.tree(text = "((A:1, B:1):2, C:3);")
}
