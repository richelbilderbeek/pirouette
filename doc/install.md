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
remotes::install_github("KlausVigo/phangorn")
remotes::install_github("ropensci/beautier")
remotes::install_github("ropensci/tracerer")
remotes::install_github("ropensci/beastier")
remotes::install_github("ropensci/mauricer")
remotes::install_github("ropensci/babette")
remotes::install_github("richelbilderbeek/mcbette")
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

