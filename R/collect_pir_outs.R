#' Collect the results of multiple \link{pirouette} runs
#' @inheritParams default_params_doc
#' @return a single \link{pir_run} output as produced by a single pir run.
#' @examples
#'
#' if (is_on_travis() && is_beast2_installed()) {
#'
#'   pir_paramses <- list()
#'   pir_paramses[[1]] <- pirouette::create_test_pir_params()
#'   pir_paramses[[2]] <- pirouette::create_test_pir_params()
#'
#'   phylogenies <- list()
#'   phylogenies[[1]] <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
#'   phylogenies[[2]] <- ape::read.tree(text = "((A:1, B:1):2, C:3);")
#'
#'   pir_outs <- pir_runs(
#'     phylogenies = phylogenies,
#'     pir_paramses = pir_paramses
#'   )
#'   pir_out_total <- collect_pir_outs(pir_outs)
#' }
#' @author Giovanni Laudanno
#' @export
collect_pir_outs <- function(pir_outs) {
  out_mat <- do.call(
    args = lapply(pir_outs, FUN = function(x) as.data.frame(x)),
    what = "rbind"
  )
  first <- pir_outs[[1]]
  rcount <- 0
  ccount <- ncol(first)
  len1 <- length(pir_outs)
  for (i in seq_len(len1)) {
    rcount <- rcount + nrow(pir_outs[[i]])
    testit::assert(ncol(pir_outs[[i]]) == ccount)
  }
  testit::assert(dim(out_mat)[1] == rcount)
  testit::assert(dim(out_mat)[2] == ccount)

  i_err_1 <- which(colnames(out_mat) == "error_1")
  i_string <- 1:(i_err_1 - 1)
  i_num <- seq_len(ncol(out_mat))[-i_string]
  out_mat$model <- interaction(out_mat$tree, out_mat$inference_model)
  model_mats <- split(out_mat, out_mat$model)
  errors <- lapply(model_mats, FUN = function(x) unlist(x[, i_num]))
  n_errors <- length(errors[[1]])

  arrange_errors <- function(y) {
    n_errors <- length(y)
    x <- data.frame(matrix(y, ncol = n_errors, nrow = 1))
    colnames(x) <- paste0("error_", 1:n_errors)
    x
  }
  errors_2 <- lapply(errors, FUN = function(x) arrange_errors(x))
  errors_3 <- do.call(
    args = lapply(errors_2, FUN = function(x) as.data.frame(x)),
    what = "rbind"
  )
  errors_3$tree <- as.factor(sub("\\..*", "", rownames(errors_3)))
  errors_3$inference_model <- as.factor(sub(".*\\.", "", rownames(errors_3)))
  errors_3$inference_model_weight <- errors_3$site_model <-
    errors_3$clock_model <- errors_3$tree_prior <- NA
  pir_out <- errors_3[, c(
    colnames(first)[i_string],
    paste0("error_", 1:n_errors))
    ]
  pir_out$site_model <- pir_out$clock_model <- pir_out$tree_prior <-
    as.factor(NA)
  rownames(pir_out) <- NULL
  return(pir_out)
}
