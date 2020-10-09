# Installation

`pirouette` can be installed

 * from script (easiest)
 * from R 

## From script

All the manual steps are put in the `install_pir_deps`
script, which is located in the `scripts` folder.

To call the script:

```
cd scripts
./install_pir_deps
```

## From R

Thanks to the `remotes` R package, this is easy:

```{r}
remotes::install_github("ropensci/mcbette", dependencies = TRUE)
remotes::install_github("richelbilderbeek/pirouette")
```

To install BEAST2, see [how to install BEAST2](https://github.com/ropensci/beastier/blob/master/install_beast2.md)
or use:

```{r}
beastier::install_beast2()
```

To install the BEAST2 `NS` package:

```{r}
mauricer::install_beast2_pkg("NS")
```

## Videos

 * How to run an R script on Peregrine [download (.ogv)](http://richelbilderbeek.nl/peregrine_call_r_script.ogv) [YouTube](https://youtu.be/Xf8IZwR9T8U)
 * How to install the pirouette R package on Peregrine [download (.ogv)](http://richelbilderbeek.nl/peregrine_install_pirouette.ogv) [YouTube](https://youtu.be/ZgAe_e7Vwy0)
 * How to run a pirouette example on Peregrine [download (.ogv)](http://richelbilderbeek.nl/peregrine_run_pirouette_example.ogv) [YouTube](https://youtu.be/wnz6l_2e_-c)

