# MortalityLaws: Parametric Mortality Models, Life Tables and HMD
[![CRAN_Version](https://www.r-pkg.org/badges/version/MortalityLaws)](https://cran.r-project.org/package=MortalityLaws)
[![Linux Build Status](https://travis-ci.org/mpascariu/MortalityLaws.svg?branch=master)](https://travis-ci.org/mpascariu/MortalityLaws)
[![codecov](https://codecov.io/github/mpascariu/MortalityLaws/branch/master/graphs/badge.svg)](https://codecov.io/github/mpascariu/MortalityLaws)
[![issues](https://img.shields.io/github/issues-raw/mpascariu/MortalityLaws.svg)](https://github.com/mpascariu/MortalityLaws/issues)

[![lifecycle](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![license](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/mpascariu/MortalityLaws/blob/master/LICENSE)
[![CRAN_Download_Badge1](https://cranlogs.r-pkg.org/badges/grand-total/MortalityLaws)](https://CRAN.R-project.org/package=MortalityLaws)
[![CRAN_Download_Badge2](https://cranlogs.r-pkg.org/badges/MortalityLaws)](https://CRAN.R-project.org/package=MortalityLaws)


Fit the most popular human mortality `laws`, and construct full and abridged life tables given various input indices. A mortality law is a parametric function that describes the dying-out process of individuals in a population during a significant portion of their 
life spans. For a comprehensive review of the most important mortality laws see [Tabeau (2001)](https://doi.org/10.1007/0-306-47562-6_1). Function for downloading demographic data from the following sources are provided:

- the [Human Mortality Database (HMD)](https://www.mortality.org/)
- the [Australian Human Mortality Database (AHMD)](https://demography.cass.anu.edu.au/research/australian-human-mortality-database)
- the [Canadian Human Mortality Database (CHMD)](http://www.bdlc.umontreal.ca/CHMD/)
- the [Japanese Mortality Database (JMD)](http://www.ipss.go.jp/p-toukei/JMD/index-en.asp)


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

## Support

<a href="https://www.buymeacoffee.com/mpascariu" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/purple_img.png" alt="Buy Me A Coffee" style="height: 41px !important;width: 174px !important;box-shadow: 0px 3px 2px 0px rgba(190, 190, 190, 0.5) !important;-webkit-box-shadow: 0px 3px 2px 0px rgba(190, 190, 190, 0.5) !important;" ></a>
