
#' Convert Life Table Indicators
#' 
#' Easy conversion between the life table indicators. This function is based 
#' on the \code{\link{LifeTable}} function and methods behind it.
#' 
#' @usage convertFx(x, data, from, to, ...)
#' @inheritParams LifeTable
#' @param data Vector or data.frame/matrix containing the mortality indicators.
#' @param from Specify the life table indicator in the input \code{data}. Character.
#' Options: \code{mx, qx, dx, lx}.   
#' @param to What indicator would you like to obtain? Character. 
#' Options: \code{mx, qx, dx, lx, Lx, Tx, ex}.
#' @param ... Further arguments to be passed to the \code{\link{LifeTable}} 
#' function with impact on the results to be produced.
#' @seealso \code{\link{LifeTable}}
#' @author Marius D. Pascariu
#' @examples 
#' # Data ---
#' x  <- 0:110
#' mx <- ahmd$mx
#' 
#' # mx to qx
#' qx <- convertFx(x, data = mx, from = "mx", to = "qx")
#' # mx to dx
#' dx <- convertFx(x, data = mx, from = "mx", to = "dx")
#' # mx to lx
#' lx <- convertFx(x, data = mx, from = "mx", to = "lx")
#' 
#' 
#' # There are 28 possible combinations --------------------------------
#' # Let generate all of them.
#' from <- c("mx", "qx", "dx", "lx")
#' to   <- c("mx", "qx", "dx", "lx", "Lx", "Tx", "ex")
#' K    <- expand.grid(from = from, to = to) # all possible cases/combinations
#' 
#' for (i in 1:nrow(K)) {
#'   In  <- as.character(K[i, "from"])
#'   Out <- as.character(K[i, "to"])
#'   N <- paste0(Out, "_from_", In)
#'   cat(i, " Create", N, "\n")
#'   # Create the 28 sets of results
#'   assign(N, convertFx(x = x, data = get(In), from = In, to = Out))
#' }
#' @export
convertFx <- function(x, data, 
                  from = c("mx", "qx", "dx", "lx"), 
                  to = c("mx", "qx", "dx", "lx", "Lx", "Tx", "ex"), ...) {
  
  from <- match.arg(from)
  to   <- match.arg(to)

  A <- switch(from,
              mx = LifeTable(x, mx = data, ...)$lt,
              qx = LifeTable(x, qx = data, ...)$lt,
              dx = LifeTable(x, dx = data, ...)$lt,
              lx = LifeTable(x, lx = data, ...)$lt
  )
  
  n <- nrow(A) / length(unique(A$x))
  
  if (n == 1) {
    B <- A[, to]
  } else {
    
    B1 <- A[, c("LT", to, "x")]
    B2 <- tidyr::spread(data = B1, key = "LT", value = to)
    B  <- B2[, -1]
    dimnames(B) <- dimnames(data)
  }
  
  return(B)
}
