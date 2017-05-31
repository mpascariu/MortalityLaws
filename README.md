# MortalityLaws
[![Build Status](https://travis-ci.org/mpascariu/MortalityLaws.svg?branch=master)](https://travis-ci.org/mpascariu/MortalityLaws)
[![license](https://img.shields.io/github/license/mpascariu/MortalityLaws.svg)]()
[![Github file size](https://img.shields.io/github/size/mpascariu/MortalityLaws.svg)]()

Fit and compare the most popular human mortality laws

This repository includes R code for fitting most popular human mortality laws
and downloading [Human Mortality Database ](http://www.mortality.org) data.
People involved in the project:
[Marius Pascariu](http://findresearcher.sdu.dk:8080/portal/da/person/mpascariu) and
[Vladimir Canudas-Romo](http://www.sdu.dk/ansat/vcanudas).

Installation
============

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

