# Code for buiding and resizing pdf vignettes


devtools::build_vignettes()
tools::compactPDF(paste0(getwd(),"/inst/doc/"), gs_quality = "ebook")

# Thu Dec 21 10:26:56 2017 ------------------------------
# Marius Pascariu

R CMD build MortalityLaws
R CMD build --compact-vignettes=gs+qpdf MortalityLaws 
R CMD CHECK --as-cran MortalityLaws_1.3.0.tar.gz