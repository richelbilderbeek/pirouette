#' Plot the error BEAST2 make from the known phylogeny
#' @param pir_out the output created by \code{\link{pir_run}}
#' @return a ggplot2 plot
#' @examples
#'   phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
#'   alignment_params <- create_alignment_params(
#'     mutation_rate = 0.01
#'   )
#'   errors <- pir_run(
#'     phylogeny = phylogeny,
#'     alignment_params = alignment_params,
#'     model_select_params = create_gen_model_select_param(
#'       alignment_params = alignment_params
#'     ),
#'     inference_param = create_inference_param(
#'       mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
#'     )
#'   )
#'   pir_plot(errors)
#' @author Richel J.C. Bilderbeek
#' @export
pir_plot <- function(pir_out) {

  df <- pir_out
  first_col_index <- which(names(df) == "error_1")
  df_long <- tidyr::gather(
    df, "error_index", "error_value", first_col_index:ncol(df)
  )

  # Satisfy R CMD check
  tree <- NULL; rm(tree) # nolint, fixes warning: no visible binding for global variable
  error_value <- NULL; rm(error_value) # nolint, fixes warning: no visible binding for global variable
  inference_model <- NULL; rm(inference_model) # nolint, fixes warning: no visible binding for global variable

  ggplot2::ggplot(
    data = df_long,
    ggplot2::aes(x = tree, y = error_value, fill = inference_model)
  ) + ggplot2::geom_violin() +
    ggplot2::ggtitle("The error BEAST2 makes")

}
