# pirouette

Branch  |[![Travis CI logo](pics/TravisCI.png)](https://travis-ci.org)                                                                            |[![AppVeyor logo](pics/AppVeyor.png)](https://www.appveyor.com)                                                                                                                   |[![Codecov logo](pics/Codecov.png)](https://www.codecov.io)
--------|-----------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------
master  |[![Build Status](https://travis-ci.org/richelbilderbeek/pirouette.svg?branch=master)](https://travis-ci.org/richelbilderbeek/pirouette)  |[![Build status](https://ci.appveyor.com/api/projects/status/vr5jkcx975w1ggcn/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/pirouette/branch/master)  |[![codecov.io](https://codecov.io/github/richelbilderbeek/pirouette/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/pirouette/branch/master)
develop |[![Build Status](https://travis-ci.org/richelbilderbeek/pirouette.svg?branch=develop)](https://travis-ci.org/richelbilderbeek/pirouette) |[![Build status](https://ci.appveyor.com/api/projects/status/vr5jkcx975w1ggcn/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/pirouette/branch/develop) |[![codecov.io](https://codecov.io/github/richelbilderbeek/pirouette/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/pirouette/branch/master)
giovanni|[![Build Status](https://travis-ci.org/richelbilderbeek/pirouette.svg?branch=giovanni)](https://travis-ci.org/richelbilderbeek/pirouette)|[![Build status](https://ci.appveyor.com/api/projects/status/vr5jkcx975w1ggcn/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/pirouette/branch/giovanni)|[![codecov.io](https://codecov.io/github/richelbilderbeek/pirouette/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/pirouette/branch/master)
richel  |[![Build Status](https://travis-ci.org/richelbilderbeek/pirouette.svg?branch=richel)](https://travis-ci.org/richelbilderbeek/pirouette)  |[![Build status](https://ci.appveyor.com/api/projects/status/vr5jkcx975w1ggcn/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/pirouette/branch/richel)  |[![codecov.io](https://codecov.io/github/richelbilderbeek/pirouette/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/pirouette/branch/master)

![](pics/pirouette_logo.png)

`pirouette` is an R package that estimates the error BEAST2 makes from a given 
phylogeny. This phylogeny can be created using any (non-BEAST) speciation model,
for example the Protracted Birth-Death or Multiple-Birth-Death models.

 * [Installation](doc/install.md).
 * [Examples](https://github.com/richelbilderbeek/pirouette_examples)
 * [Pre-print of the pirouette article](https://www.biorxiv.org/content/10.1101/2019.12.17.879098v1)
 * [Interpretation](doc/interpretation.md).

## Common abbreviations

 * `nsm`: Nucleotide Substitution Model
 * `tral`: TRin ALignment
 * `twal`: TWin ALignment

## [FAQ](doc/faq.md)

See the [FAQ](doc/faq.md).

## There is a feature I miss

See [CONTRIBUTING](CONTRIBUTING.md), at `Submitting use cases`

## I want to collaborate

See [CONTRIBUTING](CONTRIBUTING.md), at 'Submitting code'

## I think I have found a bug

See [CONTRIBUTING](CONTRIBUTING.md), at 'Submitting bugs' 

## There's something else I want to say

Sure, just add an Issue. Or send an email.

## Package dependencies

### `master` branches

Package                                                |[![Travis CI logo](pics/TravisCI.png)](https://travis-ci.org)                                                                       |[![Codecov logo](pics/Codecov.png)](https://www.codecov.io)
-------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------
[beautier](https://github.com/ropensci/beautier)       |[![Build Status](https://travis-ci.org/ropensci/beautier.svg?branch=master)](https://travis-ci.org/ropensci/beautier)               |[![codecov.io](https://codecov.io/github/ropensci/beautier/coverage.svg?branch=master)](https://codecov.io/github/ropensci/beautier/branch/master)
[beastier](https://github.com/ropensci/beastier)       |[![Build Status](https://travis-ci.org/ropensci/beastier.svg?branch=master)](https://travis-ci.org/ropensci/beastier)               |[![codecov.io](https://codecov.io/github/ropensci/beastier/coverage.svg?branch=master)](https://codecov.io/github/ropensci/beastier/branch/master)
[mauricer](https://github.com/ropensci/mauricer)       |[![Build Status](https://travis-ci.org/ropensci/mauricer.svg?branch=master)](https://travis-ci.org/ropensci/mauricer)               |[![codecov.io](https://codecov.io/github/ropensci/mauricer/coverage.svg?branch=master)](https://codecov.io/github/ropensci/mauricer/branch/master)
[mcbette](https://github.com/richelbilderbeek/mcbette) |[![Build Status](https://travis-ci.org/richelbilderbeek/mcbette.svg?branch=master)](https://travis-ci.org/richelbilderbeek/mcbette) |[![codecov.io](https://codecov.io/github/richelbilderbeek/mcbette/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/mcbette/branch/master)
[tracerer](https://github.com/ropensci/tracerer)       |[![Build Status](https://travis-ci.org/ropensci/tracerer.svg?branch=master)](https://travis-ci.org/ropensci/tracerer)               |[![codecov.io](https://codecov.io/github/ropensci/tracerer/coverage.svg?branch=master)](https://codecov.io/github/ropensci/tracerer/branch/master)

### `develop` branches

Package                                                |[![Travis CI logo](pics/TravisCI.png)](https://travis-ci.org)                                                                       |[![Codecov logo](pics/Codecov.png)](https://www.codecov.io)
-------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------
[beautier](https://github.com/ropensci/beautier)       |[![Build Status](https://travis-ci.org/ropensci/beautier.svg?branch=develop)](https://travis-ci.org/ropensci/beautier)               |[![codecov.io](https://codecov.io/github/ropensci/beautier/coverage.svg?branch=develop)](https://codecov.io/github/ropensci/beautier/branch/develop)
[beastier](https://github.com/ropensci/beastier)       |[![Build Status](https://travis-ci.org/ropensci/beastier.svg?branch=develop)](https://travis-ci.org/ropensci/beastier)               |[![codecov.io](https://codecov.io/github/ropensci/beastier/coverage.svg?branch=develop)](https://codecov.io/github/ropensci/beastier/branch/develop)
[mauricer](https://github.com/ropensci/mauricer)       |[![Build Status](https://travis-ci.org/ropensci/mauricer.svg?branch=develop)](https://travis-ci.org/ropensci/mauricer)               |[![codecov.io](https://codecov.io/github/ropensci/mauricer/coverage.svg?branch=develop)](https://codecov.io/github/ropensci/mauricer/branch/develop)
[mcbette](https://github.com/richelbilderbeek/mcbette) |[![Build Status](https://travis-ci.org/richelbilderbeek/mcbette.svg?branch=develop)](https://travis-ci.org/richelbilderbeek/mcbette) |[![codecov.io](https://codecov.io/github/richelbilderbeek/mcbette/coverage.svg?branch=develop)](https://codecov.io/github/richelbilderbeek/mcbette/branch/develop)
[tracerer](https://github.com/ropensci/tracerer)       |[![Build Status](https://travis-ci.org/ropensci/tracerer.svg?branch=develop)](https://travis-ci.org/ropensci/tracerer)               |[![codecov.io](https://codecov.io/github/ropensci/tracerer/coverage.svg?branch=develop)](https://codecov.io/github/ropensci/tracerer/branch/develop)

### Windows

Package                                                                       | Status
------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
[babette_on_windows](https://github.com/richelbilderbeek/babette_on_windows)  |[![Build status](https://ci.appveyor.com/api/projects/status/jv76errjocm5d5yq/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/babette-on-windows/branch/master)
[beastier_on_windows](https://github.com/richelbilderbeek/beastier_on_windows)|[![Build status](https://ci.appveyor.com/api/projects/status/ralex9sdnnxlwbgx/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/beastier-on-windows/branch/master)
[beautier_on_windows](https://github.com/richelbilderbeek/beautier_on_windows)|[![Build status](https://ci.appveyor.com/api/projects/status/blvjo5pulbkqxrhb/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/beautier-on-windows/branch/master)
[mauricer_on_windows](https://github.com/richelbilderbeek/mauricer_on_windows)|[![Build status](https://ci.appveyor.com/api/projects/status/bc43iwp68xo2dduh/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/mauricer-on-windows/branch/master)
[tracerer_on_windows](https://github.com/richelbilderbeek/tracerer_on_windows)|[![Build status](https://ci.appveyor.com/api/projects/status/jyhck66d6yrbr12h/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/tracerer-on-windows/branch/master)

## External links

 * [BEAST2 GitHub](https://github.com/CompEvol/beast2)
