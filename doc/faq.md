# FAQ

## Package

### What are the dependencies?

`pirouette` depends on (non-CRAN) packages like this:

![pirouette dependencies](pirouette.png)

### [How to set up the experiments](how_to_set_up_experiments.md)?

See [How to set up the experiments](how_to_set_up_experiments.md)

### Where are the files stored?

`pirouette` creates a lot of files, that are by default stored
in a `temp` folder. Here are their locations:

File                               |Where
-----------------------------------|-----------------------------------------------------------------------
The (true) phylogeny               |?someplace
The twin phylogeny                 |`pir_params$twinning_params$twin_tree_filename`
The (true) alignment               |`pir_params$alignment_params$fasta_filename`
The twin alignment                 |`pir_params$twinning_params$twin_alignment_filename`
BEAST2 (`.xml`) input file (1)     |pir_params$experiments[[1]]$beast2_options$beast2_input_filename
BEAST2 `.log` output file (1)      |pir_params$experiments[[1]]$beast2_options$beast2_output_log_filename
BEAST2 `.trees` output file (1)    |pir_params$experiments[[1]]$beast2_options$beast2_output_trees_filename
BEAST2 `.xml.state` output file (1)|pir_params$experiments[[1]]$beast2_options$beast2_output_state_filename

 * (1) of the first experiment

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
dot -Tps dependencies.dot -o dependencies.ps; convert dependencies.ps dependencies.png
```

