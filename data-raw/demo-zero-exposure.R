# --------------------------------------------
# Zero-exposure ages, seen from both sides
# Date: 2026-10-06 19:31:04
# Run from the package root: Rscript data-raw/demo-zero-exposure.R
# Not part of the build (.Rbuildignore covers data-raw/).
# --------------------------------------------
# An age with no exposure carries no information about the hazard: its observed
# rate is 0/0 or Inf. MortalityLaw() leaves those ages out of the fit and says
# so, instead of stopping. vital fits HMD-style data whose population is zero at
# the oldest ages, so the same warning reaches a vital user through its own
# smoothing wrapper. Both sides are shown below.
# --------------------------------------------

library(MortalityLaws)

# ---------------------------------------------------------------- 1. MortalityLaw
# Six ages, one of them with no person-years at risk.
x  <- 0:5
Dx <- c(100, 80, 60, 40, 20, 10)
Ex <- c(10000, 9000, 8000, 7000, 0, 3000)   # age 4 has zero exposure

cat("\n== MortalityLaw() with a zero-exposure age ==\n")
M <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "gompertz")
M
print(coef(M))
plot(M)

# The dropped age is out of the residual degrees of freedom, so
# 4 informative ages - 2 parameters.
cat("df.residual:", M$df["df.residual"], "\n")

# Everything informative dropped: the fit refuses, with its own message.
cat("\n== MortalityLaw() with nothing left to fit ==\n")
try(MortalityLaw(x = x, Dx = Dx, Ex = c(rep(0, 5), 3000), law = "gompertz"))

# ---------------------------------------------------------------- 2. vital
library(vital)

# norway_mortality carries zero population at the oldest ages.
nor <- dplyr::filter(norway_mortality, Year <= 1910, Sex == "Male")
cat("\n== the data ==\n")
cat("rows:", nrow(nor),
    "| zero-population rows:", sum(nor$Population == 0),
    "| of those with deaths > 0:", sum(nor$Population == 0 & nor$Deaths > 0),
    "\n")

# smooth_mortality_law() fits one law per year, so the warning arrives once per
# year that has a zero-exposure age: 11 years in this series. R defers repeated
# warnings and prints only a count, so the handler below shows each one as it
# comes out of MortalityLaws.
cat("\n== vital::smooth_mortality_law() on that data ==\n")
sm <- withCallingHandlers(
  vital::smooth_mortality_law(nor, Mortality),
  warning = function(w) {
    cat("  MortalityLaws said:", conditionMessage(w), "\n")
    invokeRestart("muffleWarning")
  }
)
print(utils::head(sm, 4))

cat("\nDone. The warnings above come from MortalityLaws, raised inside vital's loop.\n")


x  <- 45:75
M1 <- MortalityLaw(x = x, Dx = ahmd$Dx[as.character(x), "1950"],
                   Ex = ahmd$Ex[as.character(x), "1950"], law = "makeham")
plot(M1)
plot(M1, which = "fit")
plot(M1, which = "diagnostic")

