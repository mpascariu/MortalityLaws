
#' Convert Life Table Indicators
#' 
#' Easy conversion between the life table indicators. This function is based 
#' on the \code{\link{LifeTable}} function and methods behind it.
#' 
#' @usage convertFx(x, data, In, Out, ...)
#' @inheritParams LifeTable
#' @param data Vector or data.frame/matrix containing the mortality indicators.
#' @param In Specify what indicator did you provide in \code{data}. Options:
#' \code{mx, qx, dx, lx}.   
#' @param Out What indicator would you like to obtain? Options: 
#' \code{mx, qx, dx, lx, Lx, Tx, ex}.
#' @param ... Further arguments to be passed to the \code{\link{LifeTable}} 
#' function with impact on the results to be produced.
#' @examples 
#' # Data ---
#' x  <- 0:110
#' mx <- ahmd$mx
#' 
#' # mx to qx
#' qx <- convertFx(x, data = mx, In = "mx", Out = "qx")
#' # mx to dx
#' dx <- convertFx(x, data = mx, In = "mx", Out = "dx")
#' # mx to lx
#' lx <- convertFx(x, data = mx, In = "mx", Out = "lx")
#' 
#' 
#' # There are 28 possible combinations --------------------------------
#' # Let generate all of them.
#' In  <- c("mx", "qx", "dx", "lx")
#' Out <- c("mx", "qx", "dx", "lx", "Lx", "Tx", "ex")
#' 
#' K <- expand.grid(In = In, Out = Out) # all possible cases/combinations
#' 
#' for (i in 1:nrow(K)) {
#'   In_  <- as.character(K[i, "In"])
#'   Out_ <- as.character(K[i, "Out"])
#'   N <- paste0(Out_, "_from_", In_)
#'   cat(i, " Create", N, "\n")
#'   # Create the 28 sets of results
#'   assign(N, convertFx(x = x, data = get(In_), In = In_, Out = Out_))
#' }
#' @export
convertFx <- function(x, data, 
                  In = c("mx", "qx", "dx", "lx"), 
                  Out = c("mx", "qx", "dx", "lx", "Lx", "Tx", "ex"), ...) {
  
  In  <- match.arg(In)
  Out <- match.arg(Out)

  A <- switch(In,
              mx = LifeTable(x, mx = data, ...)$lt,
              qx = LifeTable(x, qx = data, ...)$lt,
              dx = LifeTable(x, dx = data, ...)$lt,
              lx = LifeTable(x, lx = data, ...)$lt
  )
  
  n <- nrow(A) / length(unique(A$x))
  
  if (n == 1) {
    B <- A[, Out]
  } else {
    
    B1 <- A[, c("LT", Out, "x")]
    B2 <- tidyr::spread(data = B1, key = "LT", value = Out)
    B  <- B2[, -1]
    dimnames(B) <- dimnames(data)
  }
  
  return(B)
}
