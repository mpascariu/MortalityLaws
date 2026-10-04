# --------------------------------------------
# RX1 replication of Scholey (2019)
# Date: 2026-10-04
# Run from the package root: Rscript data-raw/replicate_scholey.R
# Not part of the build (.Rbuildignore covers data-raw/).
# --------------------------------------------

## Reproduce Scholey (2019) results with this package's own laws, on the
## author's own day-level US infant life tables (his repo:
## github.com/jschoeley/parametric_infant_mortality, data/ilts.Rdata).
##
## Checks:
##  (A) Figure 3 pseudo-R2 ladder on the full first year: negative Gompertz
##      47.5%, Pareto 90.1%, shifted power 98.7%, truncated power 99.9%.
##  (B) Figure 3b: the negative Gompertz describes the post-neonatal decline
##      with a pseudo-R2 of 97.8%.
##  (C) Figure 3f: the truncated-power parameters (p ~ 0.61, b ~ 0.004,
##      c ~ 6e-4).
##  (D) the nested identities hold exactly (D = 0 -> shifted power,
##      B = 1 -> Pareto II).
##
## NOTE on the metric: the paper compares models by the POISSON deviance and a
## pseudo-R2 based on it. The package's own deviance() component is the sum of
## squared log-residuals and is NOT the Poisson deviance, so it is computed
## here directly from the fitted hazard and the observed counts/exposures.

suppressMessages(pkgload::load_all(".", quiet = TRUE))

# --- data: download the author's day-level life tables if not present --------
data_file <- "data-raw/ilts.Rdata"
if (!file.exists(data_file)) {
  url <- paste0("https://raw.githubusercontent.com/jschoeley/",
                "parametric_infant_mortality/master/data/ilts.Rdata")
  message("downloading ", url)
  utils::download.file(url, data_file, mode = "wb", quiet = TRUE)
}
e <- new.env()
load(data_file, envir = e)
ic <- get("ilt_complete", e)
x  <- ic$x      # interval start, days since birth (row 1 = age 0)
Dx <- ic$nDx    # deaths in the interval
Ex <- ic$nEx    # person-days exposed

# --- Poisson deviance and pseudo-R2, exactly as the paper defines them -------
pois_dev <- function(mu, Dx, Ex) {
  if (any(!is.finite(mu)) || any(mu <= 0)) return(Inf)
  2 * sum(Dx * log(Dx / (mu * Ex)) - (Dx - mu * Ex))
}
pseudo_r2 <- function(mu, Dx, Ex) {
  mu0 <- sum(Dx) / sum(Ex)
  null_dev <- 2 * sum(Dx * log(Dx / (mu0 * Ex)) - (Dx - mu0 * Ex))
  1 - pois_dev(mu, Dx, Ex) / null_dev
}
fit_law <- function(law, idx) {
  xs <- x[idx]; Ds <- Dx[idx]; Es <- Ex[idx]
  m  <- suppressWarnings(MortalityLaw(x = xs, Dx = Ds, Ex = Es, law = law,
                                      opt.method = "poissonL"))
  mu <- get(law, asNamespace("MortalityLaws"))(xs, coef(m))$hx
  c(r2 = pseudo_r2(mu, Ds, Es), dev = pois_dev(mu, Ds, Es))
}

# --- A. Figure 3: the nested ladder on the complete first year --------------
cat("=== A. Full infancy (his Fig. 3); paper pseudo-R2 in brackets ===\n")
paper_r2 <- c(neggompertz = 47.5, pareto_2 = 90.1,
              scholey_shifted_power = 98.7, scholey = 99.9)
all_idx <- rep(TRUE, length(x))
for (L in names(paper_r2)) {
  r <- fit_law(L, all_idx)
  cat(sprintf("  %-22s pseudo-R2 = %6.2f%%  [%.1f%%]\n", L, 100 * r[["r2"]], paper_r2[[L]]))
}

# --- B. Figure 3b: post-neonatal decline (paper: 97.8%) ---------------------
cat("\n=== B. Post-neonatal (day 30+), his Fig. 3b ===\n")
post <- x >= 30
r_gp <- fit_law("neggompertz", post)
cat(sprintf("  negative Gompertz pseudo-R2 = %.2f%%  [97.8%%]\n", 100 * r_gp[["r2"]]))
r_pt <- fit_law("pareto_2", post)
cat(sprintf("  Pareto II         pseudo-R2 = %.2f%%\n", 100 * r_pt[["r2"]]))
cat(sprintf("  => negative Gompertz beats the Pareto power law post-neonatally: %s\n",
            r_gp[["dev"]] < r_pt[["dev"]]))

# --- C. Figure 3f: truncated-power parameters -------------------------------
cat("\n=== C. Truncated-power parameters, his Fig. 3f ===\n")
m <- suppressWarnings(MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "scholey",
                                   opt.method = "poissonL"))
cf <- coef(m)
cat(sprintf("  a = %.3e [%.3e]   p = %.4f [0.61]   c = %.3e [6e-4]   b = %.4f [0.004]\n",
            cf[["A"]], exp(-8.3), cf[["B"]], cf[["C"]], cf[["D"]]))

# --- D. Nested identities (exact) -------------------------------------------
cat("\n=== D. Nested identities ===\n")
xd <- 0:365
sp <- scholey_shifted_power(xd, c(A = 2e-4, B = 0.62, C = 6e-4))$hx
tp0 <- scholey(xd, c(A = 2e-4, B = 0.62, C = 6e-4, D = 1e-14))$hx
cat(sprintf("  max |shifted_power - scholey(D~0)| = %.2e\n", max(abs(sp - tp0))))
pt <- pareto_2(xd, c(A = 2e-4, C = 6e-4))$hx
tp1 <- scholey(xd, c(A = 2e-4, B = 1, C = 6e-4, D = 1e-14))$hx
cat(sprintf("  max |pareto_2 - scholey(B=1,D~0)|   = %.2e\n", max(abs(pt - tp1))))
