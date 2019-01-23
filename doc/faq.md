# FAQ

## Package

### What are the dependencies?

`pirouette` depends on (non-CRAN) packages like this:

![pirouette dependencies](pirouette.png)

### Where is the alignment created by `pirouette` stored?

```{r}
create_alignment_params(...)$fasta_filename
```

### Where is the twin phylogeny created by `pirouette` stored?

```{r}
create_twinning_params(...)$twin_tree_filename
```

### Where is the twin alignment created by `pirouette` stored?

```{r}
create_twinning_params(...)$twin_alignment_filename
```

### Where are the BEAST2 input (`.xml`) files created by `pirouette` stored?

```{r}
pir_run(...)$beast2_input_filename
```

### Where are the BEAST2 posterior parameter estimates (`.log`) files created by `pirouette` stored?

```{r}
pir_run(...)$beast2_log_filename
```

### Where are the BEAST2 posterior trees (`.trees`) files created by `pirouette` stored?

```{r}
pir_run(...)$beast2_trees_filename
```

### Where are the BEAST2  final posterior states (`.xml.state`) files created by `pirouette` stored?

```{r}
pir_run(...)$beast2_state_filename
```

## Why the name?

`pirouette` started out as a working title, as it is an elegant
combination of `phylogeny` and `babette`. Other name contenders 
were `phyrouette`, `palette` and many more...

The name stuck.

## BEAST2

### How to install BEAST2?

See [how to install BEAST2](https://github.com/ropensci/beastier/blob/master/install_beast2.md),
or use

```{r}
beastier::install_beast2()
```

## Development and community

### How can I indicate a feature that I miss?

Submit an Issue.

### How can I submit code?

See [CONTRIBUTING](../CONTRIBUTING.md), at 'Submitting code'

### How can I submit a bug?

See [CONTRIBUTING](../CONTRIBUTING.md), at 'Submitting bugs' 

### How can I indicate something else?

Submit an Issue. Or send an email to Richel Bilderbeek.

### How to create the dependency graph from the `.dot` file?

```
dot -Tps dependencies.dot -o dependencies.ps
convert dependencies.ps dependencies.png
```

