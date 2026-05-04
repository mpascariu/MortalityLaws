# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-04 23:02:52
# --------------------------------------------

#' Compute Life Tables from Parameters of a Mortality Law
#'
#' Generate a complete life table directly from the fitted parameters of a 
#' parametric mortality model. This function evaluates the mortality law at 
#' the given ages and passes the resulting death rates (\code{mx}) or death 
#' probabilities (\code{qx}) to \code{\link{LifeTable}} for further 
#' computation of all standard life-table columns (\code{lx}, \code{dx}, 
#' \code{Lx}, \code{Tx}, \code{ex}, etc.).
#'
#' @details
#' This function is designed to work with models that have been fitted 
#' externally (e.g., via \code{\link{MortalityLaw}} or by hand). The 
#' \code{par} argument must contain the estimated coefficients of the 
#' mortality law, and \code{law} must be one of the valid codes listed by 
#' \code{\link{availableLaws}}.
#'
#' \strong{Important caveat: age scaling during fitting}
#'
#' Several mortality laws (e.g., Gompertz, Makeham) internally \emph{scale} 
#' the age vector during optimisation to ensure numerical stability. If the 
#' model was fitted using \code{MortalityLaw} over an age range \code{[a, b]}, 
#' the published coefficients correspond to the \emph{scaled} ages, not the 
#' original ages. Consequently, \code{LawTable} will only produce valid life 
#' tables for ages \eqn{\ge a} (the lower bound of the fitting range). 
#' Attempting to use the same coefficients at younger ages will yield 
#' incorrect results (e.g., life expectancy at age 25 will equal that at 
#' age 45).
#'
#' To determine which models apply age scaling, run:
#' \preformatted{
#' A <- availableLaws()$table
#' A[, c("CODE", "SCALE_X")]
#' }
#' Models with \code{SCALE_X = TRUE} rescale the age vector internally. When 
#' using \code{LawTable} with such a model, make sure the \code{x} argument 
#' starts from the same lower age bound used during fitting.
#'
#' For models that do \emph{not} scale (e.g., Heligman-Pollard \code{"HP"}), 
#' this limitation does not apply, and \code{LawTable} can be used for any 
#' age range.
#'
#' @inheritParams MortalityLaw
#' @inheritParams LifeTable
#'
#' @param par The parameters of the mortality model. Can be:
#'   \itemize{
#'     \item A numeric \strong{vector} containing the coefficients (for a 
#'           single life table).
#'     \item A numeric \strong{matrix} or \strong{data.frame} where each row 
#'           corresponds to a separate set of parameters (producing multiple 
#'           life tables). Column names should match the parameter names of 
#'           the chosen law.
#'   }
#'
#' @inherit LifeTable return details
#'
#' @seealso
#' \code{\link{LifeTable}} for constructing life tables from raw mortality 
#'   data; 
#' \code{\link{MortalityLaw}} for fitting parametric mortality models; 
#' \code{\link{availableLaws}} for the list of implemented laws and their 
#'   scaling behaviour.
#'
#' @author Marius D. Pascariu
#'
#' @examples
#' # Example 1 --- Makeham --- multiple life tables from a matrix of parameters
#'
#' x1 <- 45:100
#' L1 <- "makeham"
#' C1 <- matrix(
#'   c(0.00717, 0.07789, 0.00363,
#'     0.01018, 0.07229, 0.00001,
#'     0.00298, 0.09585, 0.00002,
#'     0.00067, 0.11572, 0.00078),
#'   nrow = 4,
#'   dimnames = list(1:4, c("A", "B", "C"))
#' )
#'
#' LawTable(x = x1, par = C1, law = L1)
#'
#' # ---- Important note on age scaling ----
#'
#' # The Makeham model applies internal age scaling during fitting.
#' # If the coefficients above were estimated over ages 45-100, the life
#' # table produced by LawTable is valid only from age 45 onward.
#'
#' # ---- Example 1B: correct usage ----
#' LawTable(x = 45:100, par = c(0.00717, 0.07789, 0.00363), law = L1)
#'
#' # ---- Example 1C: incorrect usage ----
#' # The code below uses the same coefficients but starts at age 25.
#' # Because the model was fitted on scaled ages (starting at 45),
#' # the life table at age 25 will be meaningless (e.g., e25 equals e45).
#' \dontrun{
#' LawTable(x = 25:100, par = c(0.00717, 0.07789, 0.00363), law = L1)
#' }
#'
#' # ---- How to check which laws apply scaling ----
#' A <- availableLaws()$table
#' A[, c("CODE", "SCALE_X")]
#'
#' # Example 2 --- Heligman-Pollard (no scaling) ---
#'
#' x2 <- 0:110
#' L2 <- "HP"
#' C2 <- c(0.00223, 0.01461, 0.12292, 0.00091,
#'         2.75201, 29.01877, 0.00002, 1.11411)
#'
#' LawTable(x = x2, par = C2, law = L2)
#'
#' # Because "HP" does NOT scale the age vector, the output is valid for
#' # any starting age. Compare:
#' LawTable(x = 3:110, par = C2, law = L2)
#' # Note that e3 = 70.31 in both tables, confirming consistency.
#'
#' @export
LawTable <- function(x, par, law, sex = NULL, lx0 = 1e5, ax = NULL) {

  info    <- addDetails(law)
  scale.x <- info$scale.x
  fn      <- get(law)
  xx      <- if (scale.x) scale_x(x) else x

  if (is.matrix(par) | is.data.frame(par)) {
    hx <- NULL
    for (j in 1:nrow(par)) {
      hxj <- fn(xx, par[j, ])$hx
      hx  <- cbind(hx, hxj)
    }
    dimnames(hx) <- list(x, rownames(par))

  } else {
    hx <- fn(xx, par)$hx
  }

  thisIndex  <- info$model["FIT"]

  if (thisIndex == "q[x]") {
    out <- LifeTable(x = xx, qx = hx, sex = sex, lx0 = lx0, ax = ax)
  }
  if (thisIndex == "mu[x]") {
    out <- LifeTable(x = xx, mx = hx, sex = sex, lx0 = lx0, ax = ax)
  }
  out$call <- match.call()
  return(out)
}
