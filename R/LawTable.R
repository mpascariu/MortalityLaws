
#' Compute Life Tables from Parameters of a Mortality Law
#' 
#' @inheritParams MortalityLaw
#' @inheritParams LifeTable
#' @param par The parameters of the mortality model.
#' @param scale.x Logical. Scale down \code{"x"} vector so that it begins with 
#' a small value. Method: \code{new.x = x - min(x) + 1}. Default: FALSE.
#' @inherit LifeTable return details
#' @examples 
#' 
#' # Example 1 --- Makeham --- 4 tables ----------
#' x1 = 45:100
#' L1 = "makeham"
#' C1 = matrix(c(0.00717, 0.07789, 0.00363,
#'               0.01018, 0.07229, 0.00001,
#'               0.00298, 0.09585, 0.00002,
#'               0.00067, 0.11572, 0.00078), 
#'               nrow = 4, dimnames = list(1:4, c("A", "B", "C")))
#' 
#' LawTable(x = x1, par = C1, law = L1, scale.x = TRUE)
#' 
#' # Example 2 --- Heligman-Pollard -- 1 table ----
#' x2 = 0:100
#' L2 = "HP"
#' C2 = c(0.00223, 0.01461, 0.12292, 0.00091, 
#'        2.75201, 29.01877, 0.00002, 1.11411)
#'
#'LawTable(x = x2, par = C2, law = L2, scale.x = FALSE)
#' @export
LawTable <- function(x, par, law, scale.x = FALSE, 
                     sex = NULL, lx0 = 1e+05, ax = NULL) {
  if (min(x) > 1 & (!scale.x)) {
    warning("You may want to scale down the 'x' vector so that it begins ", 
            "with a small value. Set 'scale.x = TRUE'. ", 
            "This is needed if the parameters where estimated using a scaled age vector.", 
            call. = F)
  }
  fn <- get(law)
  xx <- if (scale.x) scale_x(x) else x
  
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
  
  thisIndex  <- availableLaws(law)$table$FIT
  
  if (thisIndex == "q[x]") {
    out <- LifeTable(x, qx = hx, sex = sex, lx0 = lx0, ax = ax)
  }
  if (thisIndex == "mu[x]") {
    out <- LifeTable(x, mx = hx, sex = sex, lx0 = lx0, ax = ax)
  }
  out$call <- match.call()
  return(out)
}
