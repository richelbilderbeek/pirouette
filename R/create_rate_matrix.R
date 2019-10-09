#' Create a rate matrix for the given site model
#' @inheritParams default_params_doc
#' @return a rate matrix
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#'   created <- create_rate_matrix(site_model = create_hky_site_model())
#'
#'   expected <- matrix(nrow = 4, ncol = 4)
#'   rownames(expected) <- c("a", "c", "g", "t")
#'   colnames(expected) <- c("a", "c", "g", "t")
#'   expected[1, ] <- c(-1.00, 0.50, 0.25, 0.25)
#'   expected[2, ] <- c( 0.50,-1.00, 0.25, 0.25)
#'   expected[3, ] <- c( 0.25, 0.25,-1.00, 0.50)
#'   expected[4, ] <- c( 0.25, 0.25, 0.50,-1.00)
#'
#'   library(testthat)
#'   expect_equal(created, expected)
#' @noRd
create_rate_matrix <- function(
  site_model,
  base_frequencies = rep(0.25, 4)
) {
  implemented_models <- beautier::get_site_model_names()
  testit::assert(site_model != "node_sub")
  testit::assert("name" %in% names(site_model))
  if (!(site_model$name %in% implemented_models)) {
    stop(
      "'site_model' not implemented. \n",
      "Possible site model names: '",
        paste0(beautier::get_site_model_names(), collapse = ", "), "'. \n",
      "Actual value: '", site_model$name, "'"
    )
  }

  base_frequencies <- base_frequencies / sum(base_frequencies)
  q_matrix <- matrix(rep(base_frequencies, 4), 4, 4, byrow = TRUE)
  colnames(q_matrix) <- rownames(q_matrix) <- c("a", "c", "g", "t")

  if (site_model$name == "JC69") {
    # Jukes-Cantor 1969 model:
    #  * equal base frequencies
    #  * equal transition rates
    return(NULL)
  }
  if (site_model$name == "HKY") {
    # HKY model:
    #  * transition different from transversions
    kappa <- as.numeric(site_model$kappa)
    q_matrix[1, 2] <- q_matrix[1, 2] * kappa
    q_matrix[2, 1] <- q_matrix[2, 1] * kappa
    q_matrix[3, 4] <- q_matrix[3, 4] * kappa
    q_matrix[4, 3] <- q_matrix[4, 3] * kappa
  }
  if (site_model$name == "TN93") {
    # TN93 model:
    #  * transition different from transversions and CT != AG
    kappa_1 <- as.numeric(site_model$kappa_1_param$value)
    kappa_2 <- as.numeric(site_model$kappa_2_param$value)
    q_matrix[1, 2] <- q_matrix[1, 2] * kappa_1
    q_matrix[2, 1] <- q_matrix[2, 1] * kappa_1
    q_matrix[3, 4] <- q_matrix[3, 4] * kappa_2
    q_matrix[4, 3] <- q_matrix[4, 3] * kappa_2
  }
  if (site_model$name == "GTR") {
    # GTR model:
    #  * transition rates all different
    x <- as.numeric(
      c(
        site_model$rate_ag_param$value,
        site_model$rate_ac_param$value,
        site_model$rate_at_param$value,
        site_model$rate_cg_param$value,
        site_model$rate_gt_param$value,
        site_model$rate_ct_param$value
      )
    )
    q_matrix[1, 2:4] <- q_matrix[1, 2:4] * x[1:3]
    q_matrix[2:4, 1] <- q_matrix[2:4, 1] * x[1:3]
    q_matrix[2, 3:4] <- q_matrix[2, 3:4] * x[4:5]
    q_matrix[3:4, 2] <- q_matrix[3:4, 2] * x[4:5]
    q_matrix[3, 4] <- q_matrix[3, 4] * x[6]
    q_matrix[4, 3] <- q_matrix[4, 3] * x[6]
  }

  diag(q_matrix) <- rep(0, 4)
  for (i in 1:4) {
    q_matrix[i, i] <- -sum(q_matrix[i, ])
  }

  q_matrix
}

#' Calculate base frequencies for a (lowercase) DNA sequence
#' @inheritParams default_params_doc
#' @return a numeric vector of the four base frequencies,
#'   adenine, cytosine, guanine and thymine respectively.
#'   All values are from 0.0 (absent) to 1.0 (all bases are of the
#'   corresponding type). The sum of the four values equals 1.0
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#' expect_equal(calc_base_freq("acgt"), c(0.25, 0.25, 0.25, 0.25))
#' expect_equal(calc_base_freq("aaaa"), c(1.0, 0.0, 0.0, 0.0))
#' expect_equal(calc_base_freq("cccc"), c(0.0, 1.0, 0.0, 0.0))
#' expect_equal(calc_base_freq("gggg"), c(0.0, 0.0, 1.0, 0.0))
#' expect_equal(calc_base_freq("tttt"), c(0.0, 0.0, 0.0, 1.0))
#' @noRd
calc_base_freq <- function(
  root_sequence
) {
  f_a <- stringr::str_count(root_sequence, pattern = "a")
  f_c <- stringr::str_count(root_sequence, pattern = "c")
  f_g <- stringr::str_count(root_sequence, pattern = "g")
  f_t <- stringr::str_count(root_sequence, pattern = "t")
  freqs <- c(f_a, f_c, f_g, f_t)
  freqs <- freqs / sum(freqs)
  testit::assert(sum(freqs) == 1.0)
  freqs
}
