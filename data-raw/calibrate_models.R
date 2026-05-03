# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-03 09:28:47
# --------------------------------------------

remove(list = ls())
library(tidyverse)
library(MortalityLaws)

dt <- HMD_sample$data |> 
  filter(country == "GBRTENW", Year == 2018)
x <- dt$Age
mx <- dt$Female
names(mx) <- x

# ----------------------------------------------

# Defined a list of age ranges to apply different mortality models to specific segments:
age_range <- list(
  young = 0:15,
  accident_hump = 10:35,
  adult = 30:75,
  adult_old = 40:95,
  old = 75:95,
  all = 0:95
)

# all model pre-defined in the package
model_table <- availableLaws()$table
# Note for certain models the age range vector is scaled
# see explanation in the documentation of ?LawTable for more details.

# Model Fitting Loop

# Looped through all 30 models in availableLaws(), each with a specific age range:

# Gompertz-family (gompertz, gompertz0, invgompertz, ggompertz) → fitted to adult, adult_old, or accident_hump
# Makeham-family (makeham, makeham0, kannisto, kannisto_makeham, beard, beard_makeham) → fitted to adult or old
# Childhood models (opperman, weibull) → fitted to young
# Lifespan models (thiele, wittstein, siler, HP, HP2, HP3, HP4, rogersplanck, martinelle, carriere1, carriere2, kostaki) → fitted to all
# Old-age models (vandermaen, vandermaen2, quadratic) → fitted to adult_old or old
# Other (perks, strehler_mildvan) → fitted to adult
#
for(i in seq_len(nrow(model_table))) {
  model_name <- model_table$CODE[i]
  mort_type  <- as.numeric(model_table$TYPE[i])
  x          <- age_range[[mort_type]]
  mu         <- mx[names(mx) %in% x]
  
  M <- MortalityLaw(x, mx = mu, law = model_name, opt.method = "LF2")

  # Each model result was stored as M_<modelname>.
  assign(paste0("M_", model_name), M)

  # printed the model name, age range, and coefficients to the console for review.
  print(paste0("Model: ", model_name, ", Age-range:", paste0(range(x), collapse = "-")))
  print(coef(M))
  cat("\n\n")
  }


# Write the plots showing the calibration to a PDF file
pdf("mortality_models.pdf", width = 8, height = 6)

model_names <- ls(pattern = "^M_")

for (nm in model_names) {
  M_obj <- get(nm)
  plot(M_obj)
  mtext(nm, side = 3, line = 0.5, cex = 0.8)
}

dev.off()