# MortalityLaws
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


Warning
===============
This is work in progress! The package might be unstable and probably 
full of bugs. Use it at your own risk.
