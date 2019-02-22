# How to set up the experiments?

The `pir_run` argument `experiments` may be the most difficult
one to grasp. 

Here we show how to put some research questions into the `experiments`:

## 1. What is the error BEAST2 makes from a phylogeny using the same diversification model as it was generated by?

Here one needs only one `experiment`:

`model_type`|`run_if`        |`measure_evidence` |`beautier::inference_model`
------------|----------------|-------------------|---------------------------
`generative`|`always`        |(`TRUE` or) `FALSE`|`generative_model`

We assume we know the generative model, which
happens to be the BEAUti default:

```{r}
generative_model <- beautier::create_inference_model()
testit::assert(generative_model$site_model$name == "JC69")
testit::assert(generative_model$clock_model$name == "strict")
testit::assert(generative_model$tree_prior$name == "yule")
```

We create one experiment:

```
experiment <- create_experiment(
  model_type = "generative",
  run_if = "always",
  do_measure_evidence = FALSE,
  inference_model = generative_model
)

experiments <- list(experiment)

pir_run(
  # ...
  experiments = experiment
)
```

## 2. What is the error BEAST2 makes from a phylogeny when picking the best inference model?

To answer this question, we create as much candidate experiments as we
have inference models:

`model_type`|`run_if`        |`measure_evidence`|`beautier::inference_model`
------------|----------------|------------------|---------------------------
`candidate` |`best_candidate`|`TRUE`            |`inference_model_1`
`candidate` |`best_candidate`|`TRUE`            |`inference_model_2`
...         | ...            | ...              | ...
`candidate` |`best_candidate`|`TRUE`            |`inference_model_39`
`candidate` |`best_candidate`|`TRUE`            |`inference_model_40`

```
experiments <- create_all_experiments()
```

Running:

```
pir_run(
  # ...
  experiments = experiments
)
```

## 3. What is the error BEAST2 makes from a phylogeny, when hand-picking an inference model, compared to the background noise?

This is the same setup as the first research question, expect now we need
a twin tree.

`model_type`|`run_if`        |`measure_evidence` |`beautier::inference_model`
------------|----------------|-------------------|-----------------------------
`generative`|`always`        |(`TRUE` or) `FALSE`|`generative_model`

```r
my_favorite_inference_model <- ...
```

OK, put that in the experiment:

```r
experiment <- create_experiment(
  inference_model = my_favorite_inference_model
)

experiments <- list(experiments)
```

```r
pir_params <- create_pir_params(
  # ...
  experiments = list(),
  twinning_params = create_twinning_params
)
```

```r
pir_run(
  # ...
  pir_params = pir_params
)
```

## 4. What is the difference in error between a known generative model and the best of the other candidates?

`model_type`|`run_if`        |`measure_evidence`|`beautier::inference_model`
------------|----------------|------------------|---------------------------
`generative`|`always`        |TRUE or FALSE     |`generative_model`
`candidate` |`best_candidate`|TRUE              |Inference model 1
`candidate` |`best_candidate`|TRUE              |Inference model 2
...         | ...            | ...              | ...
`candidate` |`best_candidate`|TRUE              |Inference model 38
`candidate` |`best_candidate`|TRUE              |Inference model 39

Same as 1, but there are now two models that are run: the generative model
and the best candidate model.