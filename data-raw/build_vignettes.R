# Code for buiding and resizing pdf vignettes


devtools::build_vignettes()
tools::compactPDF(paste0(getwd(),"/inst/doc/"), gs_quality = "ebook")

# Thu Dec 21 10:26:56 2017 ------------------------------
# Marius Pascariu
