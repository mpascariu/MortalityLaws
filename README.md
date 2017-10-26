# MortalityLaws
[![Build Status](https://travis-ci.org/mpascariu/MortalityLaws.svg?branch=master)](https://travis-ci.org/mpascariu/MortalityLaws)
[![Coverage Status](https://img.shields.io/codecov/c/github/mpascariu/MortalityLaws/master.svg)](https://codecov.io/github/mpascariu/MortalityLaws?branch=master)
[![issues](https://img.shields.io/github/issues-raw/mpascariu/MortalityLaws.svg)]()
[![license](https://img.shields.io/github/license/mpascariu/MortalityLaws.svg)]()
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/MortalityLaws)](http://cran.r-project.org/package=MortalityLaws)


Fit the most popular human mortality 'laws', and construct full and abridge life tables given various input indices. A mortality law is a parametric function that describes the dying-out process of individuals in a population during a significant portion of their 
life spans. For a comprehensive review of the most important mortality laws see [Tabeau (2001)](https://doi.org/10.1007/0-306-47562-6_1). An elegant function for downloading data from [Human Mortality Database ](http://www.mortality.org) is provided as well.  


People involved in the project:
[Marius Pascariu](http://findresearcher.sdu.dk:8080/portal/da/person/mpascariu) and
[Vladimir Canudas-Romo](https://researchers.anu.edu.au/researchers/canudas-romo-v).

Installation
============

The ```MortalityLaws``` package can be installed from CRAN:

```r
install.packages("MortalityLaws")
```

The latest version is can be installed from GitHub as follows:
1. Make sure you have the most recent version of R
2. Run the following code in your R console 

```r
# install.packages("devtools")

library(devtools)
install_github("mpascariu/MortalityLaws")
```

Help
===============
All functions are documented in the standard way, which means that 
once you load the package using ```library(MortalityLaws)```
you can just type ```?MortalityLaw``` to see the help file. 

