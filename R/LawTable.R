# --------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Mon Nov 25 19:35:40 2019
# --------------------------------------------------- #


#' Compute Life Tables from Parameters of a Mortality Law
#'
#' @inheritParams MortalityLaw
#' @inheritParams LifeTable
#' @param par The parameters of the mortality model.
#' @inherit LifeTable return details
#' @seealso
#' \code{\link{LifeTable}}
#' \code{\link{MortalityLaw}}
#' @author Marius D. Pascariu
#' @examples
#' # Example 1 --- Makeham --- 4 tables ----------
#' x1 = 45:100
#' L1 = "makeham"
#' C1 = matrix(c(0.00717, 0.07789, 0.00363,
#'               0.01018, 0.07229, 0.00001,
#'               0.00298, 0.09585, 0.00002,
#'               0.00067, 0.11572, 0.00078),
#'             nrow = 4, dimnames = list(1:4, c("A", "B", "C")))
#'
#' LawTable(x = 45:100, par = C1, law = L1)
#'
#' # WARNING!!!
#'
#' # It is important to know how the coefficients have been estimated. If the
#' # fitting of the model was done over the [x, x+) age-range, the LawTable
#' # function can be used to create a life table only for age x onward.
#'
#' # What can go wrong?
#'
#' # ** Example 1B - is OK.
#' LawTable(x = 45:100, par = c(0.00717, 0.07789, 0.00363), law = L1)
#'
#' # ** Example 1C - Not OK, because the life expectancy at age 25 is
#' # equal with life expectancy at age 45 in the previous example.
#' LawTable(x = 25:100, par = c(0.00717, 0.07789, 0.00363), law = L1)
#'
#' # Why is this happening?
#'
#' # If we have a model that covers only a part of the human mortality curve
#' # (e.g. adult mortality), in fitting the x vector is scaled down, meaning
#' # age (x) becomes (x - min(x) + 1). And, the coefficients are estimated on
#' # a scaled x in ordered to obtain meaningful estimates. Otherwise the
#' # optimization process might not converge.
#'
#' # What can we do about it?
#'
#' # a). Know which mortality laws are rescaling the x vector in the fitting
#' # process. If these models are fitted with the MortalityLaw() function, you
#' # can find out like so:
#' A <- availableLaws()$table
#' A[, c("CODE", "SCALE_X")]
#'
#' # b). If you are using one of the models that are applying scaling,
#' # be aware over what age-range the coefficients have been estimated. If they
#' # have been estimated using, say, ages 50 to 80, you can use the
#' # LawTable() to build a life tables from age 50 onwards.
#'
#'
#' # Example 2 --- Heligman-Pollard -- 1 table ----
#' x2 = 0:110
#' L2 = "HP"
#' C2 = c(0.00223, 0.01461, 0.12292, 0.00091,
#'        2.75201, 29.01877, 0.00002, 1.11411)
#'
#' LawTable(x = x2, par = C2, law = L2)
#'
#' # Because "HP" is not scaling down the x vector, the output is not affected
#' # by the problem described above.
#'
#' # Check
#' LawTable(x = 3:110, par = C2, law = L2)
#' # Note the e3 = 70.31 in both tables
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
