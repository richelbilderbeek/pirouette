# pirouette

Branch|[![Travis CI logo](pics/TravisCI.png)](https://travis-ci.org)|[![AppVeyor logo](pics/AppVeyor.png)](https://www.appveyor.com)|[![Codecov logo](pics/Codecov.png)](https://www.codecov.io)
---|---|---|---
master|[![Build Status](https://travis-ci.org/richelbilderbeek/pirouette.svg?branch=master)](https://travis-ci.org/richelbilderbeek/pirouette)|[![Build status](https://ci.appveyor.com/api/projects/status/vr5jkcx975w1ggcn/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/pirouette/branch/master)|[![codecov.io](https://codecov.io/github/richelbilderbeek/pirouette/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/pirouette/branch/master)
develop|[![Build Status](https://travis-ci.org/richelbilderbeek/pirouette.svg?branch=develop)](https://travis-ci.org/richelbilderbeek/pirouette)|[![Build status](https://ci.appveyor.com/api/projects/status/vr5jkcx975w1ggcn/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/pirouette/branch/develop)|[![codecov.io](https://codecov.io/github/richelbilderbeek/pirouette/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/pirouette/branch/master)
giovanni|[![Build Status](https://travis-ci.org/richelbilderbeek/pirouette.svg?branch=giovanni)](https://travis-ci.org/richelbilderbeek/pirouette)|[![Build status](https://ci.appveyor.com/api/projects/status/vr5jkcx975w1ggcn/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/pirouette/branch/giovanni)|[![codecov.io](https://codecov.io/github/richelbilderbeek/pirouette/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/pirouette/branch/master)
richel|[![Build Status](https://travis-ci.org/richelbilderbeek/pirouette.svg?branch=richel)](https://travis-ci.org/richelbilderbeek/pirouette)|[![Build status](https://ci.appveyor.com/api/projects/status/vr5jkcx975w1ggcn/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/pirouette/branch/richel)|[![codecov.io](https://codecov.io/github/richelbilderbeek/pirouette/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/pirouette/branch/master)

`pirouette` is an R package that estimates the error BEAST2 makes from a given 
phylogeny. This phylogeny can be created using any (non-BEAST) speciation model,
for example the Protracted Birth-Death or Multiple-Birth-Death models.

The heavy lifting is done by these packages:

 * [phangorn](https://github.com/KlausVigo/phangorn): to simulate an alignment from a phylogeny
 * [babette](https://github.com/ropensci/babette): to use an alignment to create a posterior

## Installation

If you use the `devtools` R package, this is easy:

```{r}
devtools::install_github("KlausVigo/phangorn", ref = "devel")
devtools::install_github("richelbilderbeek/pirouette")
```

To install the non-CRAN prerequisites, do this:

```{r}
devtools::install_github("ropensci/beautier")
devtools::install_github("ropensci/tracerer")
devtools::install_github("ropensci/beastier")
devtools::install_github("ropensci/mauricer")
devtools::install_github("ropensci/babette")
devtools::install_github("richelbilderbeek/mcbette")
```

To install BEAST2, see [how to install BEAST2](https://github.com/ropensci/beastier/blob/master/install_beast2.md)
or use:

```{r}
beastier::install_beast2()
```

## [FAQ](docs/faq.md)

See the [FAQ](docs/faq.md).

## There is a feature I miss

See [CONTRIBUTING](CONTRIBUTING.md), at `Submitting use cases`

## I want to collaborate

See [CONTRIBUTING](CONTRIBUTING.md), at 'Submitting code'

## I think I have found a bug

See [CONTRIBUTING](CONTRIBUTING.md), at 'Submitting bugs' 

## There's something else I want to say

Sure, just add an Issue. Or send an email.

## Package dependencies

Package|[![Travis CI logo](pics/TravisCI.png)](https://travis-ci.org)|[![Codecov logo](pics/Codecov.png)](https://www.codecov.io)
---|---|---
[babette](https://github.com/ropensci/babette)|[![Build Status](https://travis-ci.org/ropensci/babette.svg?branch=master)](https://travis-ci.org/ropensci/babette)|[![codecov.io](https://codecov.io/github/ropensci/babette/coverage.svg?branch=master)](https://codecov.io/github/ropensci/babette/branch/master)
[beautier](https://github.com/ropensci/beautier)|[![Build Status](https://travis-ci.org/ropensci/beautier.svg?branch=master)](https://travis-ci.org/ropensci/beautier)|[![codecov.io](https://codecov.io/github/ropensci/beautier/coverage.svg?branch=master)](https://codecov.io/github/ropensci/beautier/branch/master)
[beastier](https://github.com/ropensci/beastier)|[![Build Status](https://travis-ci.org/ropensci/beastier.svg?branch=master)](https://travis-ci.org/ropensci/beastier)|[![codecov.io](https://codecov.io/github/ropensci/beastier/coverage.svg?branch=master)](https://codecov.io/github/ropensci/beastier/branch/master)
[mauricer](https://github.com/ropensci/mauricer)|[![Build Status](https://travis-ci.org/ropensci/mauricer.svg?branch=master)](https://travis-ci.org/ropensci/mauricer)|[![codecov.io](https://codecov.io/github/ropensci/mauricer/coverage.svg?branch=master)](https://codecov.io/github/ropensci/mauricer/branch/master)
[mcbette](https://github.com/richelbilderbeek/mcbette)|[![Build Status](https://travis-ci.org/richelbilderbeek/mcbette.svg?branch=master)](https://travis-ci.org/richelbilderbeek/mcbette)|[![codecov.io](https://codecov.io/github/richelbilderbeek/mcbette/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/mcbette/branch/master)
[phangorn](https://github.com/KlausVigo/phangorn)|[![Build Status](https://travis-ci.org/KlausVigo/phangorn.svg?branch=master)](https://travis-ci.org/KlausVigo/phangorn)|[![codecov.io](https://codecov.io/github/KlausVigo/phangorn/coverage.svg?branch=master)](https://codecov.io/github/KlausVigo/phangorn/branch/master)
[tracerer](https://github.com/ropensci/tracerer)|[![Build Status](https://travis-ci.org/ropensci/tracerer.svg?branch=master)](https://travis-ci.org/ropensci/tracerer)|[![codecov.io](https://codecov.io/github/ropensci/tracerer/coverage.svg?branch=master)](https://codecov.io/github/ropensci/tracerer/branch/master)

## External links

 * [BEAST2 GitHub](https://github.com/CompEvol/beast2)

