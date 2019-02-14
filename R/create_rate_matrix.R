#' Create a rate matrix for the given site model
#' @inheritParams default_params_doc
#' @return a rate matrix
#' @author Giovanni Laudanno
#' @export
create_rate_matrix <- function(
  site_model,
  base_frequencies = rep(0.25, 4)
) {

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
    #  * equal base frequencies (STUB)
    #  * transition different from transversions
    kappa <- as.numeric(site_model$kappa)
    q_matrix[1, 2] <- q_matrix[1, 2] * kappa
    q_matrix[2, 1] <- q_matrix[2, 1] * kappa
    q_matrix[3, 4] <- q_matrix[3, 4] * kappa
    q_matrix[4, 3] <- q_matrix[4, 3] * kappa
  }
  if (site_model$name == "TN93") {
    # TN93 model:
    #  * equal base frequencies (STUB)
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
    #  * equal base frequencies (STUB)
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
