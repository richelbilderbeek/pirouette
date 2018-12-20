# pirouette

Branch|[![Travis CI logo](pics/TravisCI.png)](https://travis-ci.org)|[![AppVeyor logo](pics/AppVeyor.png)](https://www.appveyor.com)|[![Codecov logo](pics/Codecov.png)](https://www.codecov.io)
---|---|---|---
master|[![Build Status](https://travis-ci.org/richelbilderbeek/pirouette.svg?branch=master)](https://travis-ci.org/richelbilderbeek/pirouette)|[![Build status](https://ci.appveyor.com/api/projects/status/vr5jkcx975w1ggcn/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/pirouette/branch/master)|[![codecov.io](https://codecov.io/github/richelbilderbeek/pirouette/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/pirouette/branch/master)
develop|[![Build Status](https://travis-ci.org/richelbilderbeek/pirouette.svg?branch=develop)](https://travis-ci.org/richelbilderbeek/pirouette)|[![Build status](https://ci.appveyor.com/api/projects/status/vr5jkcx975w1ggcn/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/pirouette/branch/develop)|[![codecov.io](https://codecov.io/github/richelbilderbeek/pirouette/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/pirouette/branch/master)



`pirouette` is an R package that estimates the error BEAST2 makes from a given 
phylogeny. This phylogeny can be created using any (non-BEAST) speciation model,
for example the Protracted Birth-Death or Multiple-Birth-Death models.

The heavy lifting is done by these packages:

 * [phangorn](https://github.com/KlausVigo/phangorn): to simulate an alignment from a phylogeny
 * [babette](https://github.com/richelbilderbeek/babette): to use an alignment to create a posterior

## Installation

If you use the `devtools` R package, this is easy:

```
devtools::install_github("KlausVigo/phangorn", ref = "devel")
devtools::install_github("richelbilderbeek/pirouette")
```

To install BEAST2, see [how to install BEAST2](https://github.com/richelbilderbeek/beastier/blob/master/install_beast2.md).

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
[babette](https://github.com/richelbilderbeek/babette)|[![Build Status](https://travis-ci.org/richelbilderbeek/babette.svg?branch=master)](https://travis-ci.org/richelbilderbeek/babette)|[![codecov.io](https://codecov.io/github/richelbilderbeek/babette/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/babette/branch/master)
[beautier](https://github.com/richelbilderbeek/beautier)|[![Build Status](https://travis-ci.org/richelbilderbeek/beautier.svg?branch=master)](https://travis-ci.org/richelbilderbeek/beautier)|[![codecov.io](https://codecov.io/github/richelbilderbeek/beautier/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/beautier/branch/master)
[beastier](https://github.com/richelbilderbeek/beastier)|[![Build Status](https://travis-ci.org/richelbilderbeek/beastier.svg?branch=master)](https://travis-ci.org/richelbilderbeek/beastier)|[![codecov.io](https://codecov.io/github/richelbilderbeek/beastier/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/beastier/branch/master)
[phangorn](https://github.com/KlausVigo/phangorn)|[![Build Status](https://travis-ci.org/KlausVigo/phangorn.svg?branch=master)](https://travis-ci.org/KlausVigo/phangorn)|[![codecov.io](https://codecov.io/github/KlausVigo/phangorn/coverage.svg?branch=master)](https://codecov.io/github/KlausVigo/phangorn/branch/master)
[tracerer](https://github.com/richelbilderbeek/tracerer)|[![Build Status](https://travis-ci.org/richelbilderbeek/tracerer.svg?branch=master)](https://travis-ci.org/richelbilderbeek/tracerer)|[![codecov.io](https://codecov.io/github/richelbilderbeek/tracerer/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/tracerer/branch/master)

## External links

 * [BEAST2 GitHub](https://github.com/CompEvol/beast2)

