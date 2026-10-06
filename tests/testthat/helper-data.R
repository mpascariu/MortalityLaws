# --------------------------------------------
# Shared inputs for the test suite.
# --------------------------------------------
# The grids below were copy-pasted into four test files (17 copies of the same
# rate vector); they live here so no copy can drift. `ahmd` is the package's own
# Swedish example data, so the accessors return the columns the fits carry.
# --------------------------------------------

# 0-105 single ages: the schedule of the conversion tests.
grid_single <- 0:105

# Abridged schedules: ages 0 and 1, then five-year groups.
grid_ab_75  <- c(0, 1, seq(5, 75, by = 5))
grid_ab_100 <- c(0, 1, seq(5, 100, by = 5))
grid_ab_110 <- c(0, 1, seq(5, 110, by = 5))

# A plausible rate vector for each abridged grid, rising to old-age mortality.
# Every rate is finite, positive and below 1, so no guard fires.
mx_ab_75  <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
               .004, .005, .006, .0093, .0129, .019, .031, .049, .084)
mx_ab_100 <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
               .004, .005, .006, .0093, .0129, .019, .031, .049,
               .084, .129, .180, .2354, .3085, .390)
mx_ab_110 <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
               .004, .005, .006, .0093, .0129, .019, .031, .049,
               .084, .129, .180, .2354, .3085, .390, .478, .551)

# A six-age toy grid, small enough to reason about by hand.
grid_small <- 0:5
mx_small   <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06)

# AHMD (Sweden), one column, ages named.
mx_1950 <- function(x) ahmd$mx[paste(x), "1950"]
Dx_1950 <- function(x) ahmd$Dx[paste(x), "1950"]
Ex_1950 <- function(x) ahmd$Ex[paste(x), "1950"]

# Strip the convertFx class and metadata, keeping names and shape. Value
# comparisons against plain numbers live outside the class contract.
vals <- function(z) {
  attributes(z) <- attributes(z)[c("names", "dim", "dimnames")]
  z
}

# plot() auto-opens the default device (and drops Rplots.pdf) when none is
# active.
with_pdf_device <- function(expr) {
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  return(expr)
}

# Table builders report what they repaired (the open interval they closed, the
# ages they left out). The suite does not test those notes, so they are
# silenced together here rather than at each of the call sites.
quiet <- function(x) {
  suppressMessages(suppressWarnings(x))
}
