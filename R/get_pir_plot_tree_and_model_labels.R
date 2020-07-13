#' Internal function to obtain the \link{pir_plot} legend labels
#' @param df_long the output created by \code{\link{pir_run}} in the long form
#' @return the \link{pir_plot} legend labels
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @export
get_pir_plot_tree_and_model_labels <- function(df_long) {# nolint long function name is fine for an internal function
  # Get the 'model_setting' (e.g. JC, RLN, BD) from the 'tree_and_model'
  # (e.g. 'true_generative')
  get_first <- function(x) utils::head(x, n = 1)
  # True, Generative
  tg_label <- NULL
  tg_model <- get_first(
    df_long$model_setting[df_long$tree_and_model == "true_generative"]
  )
  if (length(tg_model)) {
    tg_label <- paste("Generative, true:", tg_model)
  }
  # Twin, Generative
  wg_label <- NULL
  wg_model <- get_first(
    df_long$model_setting[df_long$tree_and_model == "twin_generative"]
  )
  if (length(wg_model)) {
    wg_label <- paste("Generative, twin:", wg_model)
  }
  # True, Best
  tb_label <- NULL
  tb_model <- get_first(
    df_long$model_setting[df_long$tree_and_model == "true_candidate"]
  )
  if (length(tb_model)) {
    tb_label <- paste("Best, true:", tb_model)
  }
  # Twin, Best
  wb_label <- NULL
  wb_model <- get_first(
    df_long$model_setting[df_long$tree_and_model == "twin_candidate"]
  )
  if (length(wb_model)) {
    wb_label <- paste("Best, twin:", wb_model)
  }

  # Collect all labels. Absent models have NULL labels and are thus ignored
  tree_and_model_labels <- c(
    tg_label,
    wg_label,
    tb_label,
    wb_label
  )
  tree_and_model_labels
}
