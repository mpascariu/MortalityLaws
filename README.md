# MortalityLaws: Parametric Mortality Models, Life Tables and HMD
[![CRAN_Version](https://www.r-pkg.org/badges/version/MortalityLaws)](https://cran.r-project.org/package=MortalityLaws)
[![Linux Build Status](https://travis-ci.org/mpascariu/MortalityLaws.svg?branch=master)](https://travis-ci.org/mpascariu/MortalityLaws)
[![codecov](https://codecov.io/github/mpascariu/MortalityLaws/branch/master/graphs/badge.svg)](https://codecov.io/github/mpascariu/MortalityLaws)
[![issues](https://img.shields.io/github/issues-raw/mpascariu/MortalityLaws.svg)](https://github.com/mpascariu/MortalityLaws/issues)
[![license](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://github.com/mpascariu/MortalityLaws/blob/master/LICENSE)
[![CRAN_Download_Badge1](https://cranlogs.r-pkg.org/badges/grand-total/MortalityLaws)](https://CRAN.R-project.org/package=MortalityLaws)
[![CRAN_Download_Badge2](https://cranlogs.r-pkg.org/badges/MortalityLaws)](https://CRAN.R-project.org/package=MortalityLaws)


Fit the most popular human mortality `laws`, and construct full and abridge life tables given various input indices. A mortality law is a parametric function that describes the dying-out process of individuals in a population during a significant portion of their 
life spans. For a comprehensive review of the most important mortality laws see [Tabeau (2001)](https://doi.org/10.1007/0-306-47562-6_1). An elegant function for downloading data from [Human Mortality Database ](http://www.mortality.org) is provided as well.  


## Installation

1. Make sure you have the most recent version of R
2. Run the following code in your R console 

```R
install.packages("MortalityLaws")
```

## Updating to the latest version of the package

You can track and contribute to the development of `MortalityLaws` on [GitHub](https://github.com/mpascariu/MortalityLaws). To install it:

1. Install the release version of `devtools` from CRAN with `install.packages("devtools")`.

2. Make sure you have a working development environment.
    * **Windows**: Install [Rtools](https://CRAN.R-project.org/bin/windows/Rtools/).
    * **Mac**: Install `Xcode` from the Mac App Store.
    * **Linux**: Install a compiler and various development libraries (details vary across different flavors of Linux).

3. Install the development version of `MortalityLaws`.

   ```R
   devtools::install_github("mpascariu/MortalityLaws")
   ```

## Help
All functions are documented in the standard way, which means that once you load the package using ```library(MortalityLaws)``` you can just type ```?MortalityLaw``` to see the help file. 

