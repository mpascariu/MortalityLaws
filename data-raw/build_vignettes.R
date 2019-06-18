# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: MIT
# Last update: Tue Jun 18 12:29:22 2019
# --------------------------------------------------- #

# Code for buiding and resizing pdf vignettes


devtools::build_vignettes()
tools::compactPDF(paths = paste0(getwd(),"/inst/doc/"),
                  gs_quality = "ebook")
# If this returns NULL make sure that you have GHostscript installed and add
# added to the PATH of the system


# Thu Dec 21 10:26:56 2017 ------------------------------
# Marius Pascariu

R CMD build MortalityLaws
R CMD build --compact-vignettes=gs+qpdf MortalityLaws
R CMD CHECK --as-cran MortalityLaws_1.7.6.tar.gz
