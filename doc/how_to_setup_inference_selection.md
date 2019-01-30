# How to set up inference selection?

With `pirouette` there are multiple ways to
select models to do an inference with.

## 1. Only run the generative model

model_select_params <- create_model_select_params(
  type = "generative",
  run_if = "always",
  do_measure_evidence = FALSE, # This value is actually irrelevant
  inference_params = create_inference_params(
    site_model = create_jc69_site_model(),
    clock_model = create_strict_clock_model(),
    tree_prior = create_bd_tree_prior(),
    mrca_prior = NA,
    mcmc = create_mcmc()
  )
)


## 2. Only run the best of a set of models

## 3. Always run a generative model, 
 run the best of a set of models


Need ONE call to pirouette

inference_model_params{ # BEAST2 params?
  site_model, clock_model, tree_prior,
  BEAST2 filenames, # Essential for raket/razzo
  RNG seed
}

model_select_params{
  generative_model = ..., # Will always run
  candidate_models = list(...),
  selection = "best_candidate", 
  twinning = TRUE/FALSE
}

selection:
 * best_candidate, if best in candidate models


Run
 * 1. always
 * 2. 
 * 3. if_best_all, if best in all

{ 
  {run_if, inference_params}
}

1. 

{
  {always, gen. model}
}

2. 

{
  {always, gen. model},
  {if_best, model 1},
  {if_best, model 2}
}

3. 

{
  {always     , gen. model},
  {if_best_all, model 1},
  {if_best_all, model 2}
}

If model 1 has more evidence than gen. model, 
run gen. model and model 1.

If gen. model has most evidence, 
run gen. model.


Determine evidence of all models

