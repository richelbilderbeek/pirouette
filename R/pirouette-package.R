#' pirouette: A package to estimate the error BEAST2
#' makes in inferring a phylogeny.
#'
#' 'pirouette' is an R package that estimates the error BEAST2 makes
#' from a given phylogeny. This phylogeny can be created using any (non-BEAST)
#' speciation model, for example the Protracted Birth-Death
#' or Multiple-Birth-Death models. 'pirouette' is presented in the article
#' (in press) "pirouette: the error BEAST2 makes in inferring a phylogeny"
#' authored by R. J. C. Bilderbeek, G. Laudanno and R. S. Etienne.
#'
#' @examples
#' if (beautier::is_on_ci()) {
#'
#'   phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
#'
#'   # Select all experiments with 'run_if' is 'always'
#'   experiment <- create_test_gen_experiment()
#'   experiments <- list(experiment)
#'
#'   pir_params <- create_pir_params(
#'     alignment_params = create_test_alignment_params(),
#'     experiments = experiments
#'   )
#'
#'   if (rappdirs::app_dir()$os != "win" && beastier::is_beast2_installed()) {
#'     pir_run(
#'       phylogeny = phylogeny,
#'       pir_params = pir_params
#'     )
#'   } else {
#'     create_test_pir_run_output()
#'   }
#' }
#' @note
#'   These abbeviations are commonly used throughout the package:
#'   \itemize{
#'     \item `nsm` Nucleotide Substitution Model
#'     \item `tral`: TRue ALignment
#'     \item `trtr`: TRue TRee
#'     \item `twal`: TWin ALignment
#'     \item `twtr`: TWin TRee
#'   }
#'
#' @seealso
#'
#' \itemize{
#'   \item To simulate an alignment, use \link{sim_alignment_with_std_nsm}
#'     or \link{sim_alignment_with_n_mutations}.
#'   \item To simulate a true alignment, see \link{check_sim_tral_fun}
#'     for an overview of functions.
#'   \item To simulate a twin alignment, see \link{check_sim_twal_fun}
#'     for an overview of functions.
#'   \item To simulate a twin tree, see \link{check_sim_twin_tree_fun}
#'     for an overview of functions.
#' }
#'
#'
#' These are packages associated with \link{pirouette}:
#' \itemize{
#'   \item{
#'     \link{babette}: work with BEAST2
#'   }
#'   \item{
#'     \link{beautier}: create BEAST2 input files
#'   }
#'   \item{
#'     \link{beastier}: run BEAST2
#'   }
#'   \item{
#'     \link{mauricer}: install BEAST2 packages
#'   }
#'   \item{
#'     \code{mcbette}: compare inference models
#'   }
#'   \item{
#'     \link{tracerer}: parse and analyse BEAST2 output
#'   }
#' }
#' @author Richèl J.C. Bilderbeek
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
