# News

Newest versions at top.

## `pirouette` 1.6.6 (unreleased)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * None

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## `pirouette` 1.6.5 (unreleased)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * Fix `Imports` [#432](https://github.com/richelbilderbeek/pirouette/issues/432)
 * Fix `sim_true_alignment` example, 
   that fails on Mac [#432](https://github.com/richelbilderbeek/pirouette/issues/432)

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## `pirouette` 1.6.4 (2020-11-24)

### NEW FEATURES

 * Added `check_pir_paramses` to check a list of `pir_params` for validity
 * Add `create_std_pir_paramses` and `create_std_pir_params` to easily create
   one or more `pir_params` in a standard way
 * Add `shorten_pir_params` and `shorten_pir_paramses` to shorten run-time
   (for example, on Travis CI) easily
 * Add `pir_save` to save all pirouette output in a standarized way

### MINOR IMPROVEMENTS

 * None

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## `pirouette` 1.6.3 (2020-02-26)

### NEW FEATURES

 * Added `pir_rename_to_std` to conformize all files created by a pirouette run
 * Added 'pir_rename' to rename all files created by a pirouette run
 * Depend on beautier v2.3.5 (for `rename_mcmc_filenames`)

### MINOR IMPROVEMENTS

 * Use at-least versions of packages, instead of exact versions
 * Check if there is need for an evidence file. Will stop if there
   is an evidence filename specified without needing one and vice versa

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * Use new `nodeSube` v0.5 (2020-01-13) default arguments

## `pirouette` 1.6.2 (2020-01-06)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * Process `goodpractice` feedback

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## `pirouette` 1.6.1 (2020-01-01)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * Improved documentation
 * Increased code coverage
 * Removed dead code

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## `pirouette` 1.6 (2019-10-13)

### NEW FEATURES

 * Can create a twin tree that is a copy, by using `copy_tree` as the
   `method` used in `create_twin_tree`
 * Can use node substitution model, by using `linked_node_sub (lns)` 
   or `unlinked_node_sub (uns)` as the `site_model` used in `create_alignment`

### MINOR IMPROVEMENTS

 * None

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## `pirouette` 1.5.1 (2019-09-10)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * Use `mcbette` version in which a higher number of particles in 
   Nested Sampling does result in a better marginal likelihood
   estimation

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## `pirouette` 1.5 (2019-08-19)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * Checks that all experiments have a unique inference model

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## `pirouette` 1.4.1 (2019-08-19)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * None

### BUG FIXES

 * Table with evidences for twin models works

### DEPRECATED AND DEFUNCT

 * None

## `pirouette` 1.4 (2019-08-18)

### NEW FEATURES

 * Evidence estimations shows estimated error of evidence
 * Evidence estimations shows effective sample size in estimating the evidence

### MINOR IMPROVEMENTS

 * Better error message when using a CBS site model and too few taxa

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## `pirouette` 1.3 (2019-08-15)

### NEW FEATURES

 * Removed duplicate `evidence_epsilon` argument from `est_evidences`:
   use the `epsilon` supplied in the experiments' `est_evidence_mcmc$epsilon`

### MINOR IMPROVEMENTS

 * Better error message when using a CBS site model and too few taxa

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None
