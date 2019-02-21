# Goal: find the ideal tree,
# that is, the tree that will give the lowest error

# Approach:
# * use the most stunning tree ever created in living memory
# * do a pirouette run, and measure its error
library(pirouette)
library(ggplot2)

n_tips <- 5
dna_length <- 1000
mcmc_chain_length = 1000000
burn_in_fraction <- 0.2

pir_params <- create_pir_params(
  alignment_params = create_alignment_params(
    root_sequence = create_blocked_dna(length = dna_length),
    mutation_rate = create_standard_mutation_rate
  ),
  twinning_params = create_twinning_params(),
  experiments = list(
    create_experiment(
      inference_model = create_inference_model(
        mcmc = create_mcmc(
          chain_length = mcmc_chain_length,
          store_every = 1000
        )
      )
    )
  )
)

# Create an ideal tree
ideal_tree <- create_stunning_tree()
testit::assert(ape::Ntip(ideal_tree) == n_tips)
ape::plot.phylo(ideal_tree)

# Determine its error
errors <- pir_run(
  phylogeny = ideal_tree,
  pir_params = pir_params
)

# Convert to tidy data
first_error_col <- which(colnames(errors) == "error_1")
last_error_col <- ncol(errors)
n_errors <- 1 + last_error_col - first_error_col
df_all <- tidyr::gather(data = errors, key = "state_index", value = "error", first_error_col:last_error_col)

# Remove burn-in
df <- tracerer::remove_burn_ins(df_all, burn_in_fraction = burn_in_fraction)

ggplot(df, aes(error, fill = tree)) + geom_histogram(position = "identity", alpha = 0.5) +
  geom_vline(xintercept = mean(df$error[ df$tree == "true"]), color = "red") +
  geom_vline(xintercept = mean(df$error[ df$tree == "twin"]), color = "blue")

ggplot(df, aes(error, fill = tree)) + geom_density(alpha = 0.5) +
  geom_vline(xintercept = mean(df$error[ df$tree == "true"]), color = "red") +
  geom_vline(xintercept = mean(df$error[ df$tree == "twin"]), color = "blue")
