# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: MIT
# Last update: Wed Jun 05 14:31:12 2019
# --------------------------------------------------- #

# Code for buiding and resizing pdf vignettes


devtools::build_vignettes()
tools::compactPDF(paste0(getwd(),"/inst/doc/"), gs_quality = "ebook")

# Thu Dec 21 10:26:56 2017 ------------------------------
# Marius Pascariu

R CMD build MortalityLaws
R CMD build --compact-vignettes=gs+qpdf MortalityLaws
R CMD CHECK --as-cran MortalityLaws_1.7.6.tar.gz
