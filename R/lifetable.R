
#' Life table function
#' 
#' Function to create a life table with input variables: (age, Dx, Ex) 
#' or (age, mx) or (age, qx)
#' @param x vector of ages
#' @param Dx Dx
#' @param Ex Ex
#' @param mx mx
#' @param qx qx
#' @param lx0 lx0
#' @return Results
#' @export

lifetable <- function(x , Dx = NULL, Ex = NULL, mx = NULL, qx = NULL, 
                      lx0 = 100000){
     nmax   <- length(x)
     n      <- rep(1,nmax)           # width of the intervals
     ax     <- n/2
     ax[1]  <- ifelse(x[1] == 0, .1, .5)
     
     mx	   <- if(length(Dx) > 0) { Dx/Ex } 
                    else { 
                         if(length(mx)>0) { mx } 
                         else { qx/(n-qx*(n-ax)) 
                         }   
                    }
     if(mx[nmax]<0.5) mx[nmax] = 0.5 # In small populations we could have problems 
     # in estimating a reliable mx at last age in the lifetable
     ax[nmax] <- 1/mx[nmax]
     
     qx       <- if(length(qx) > 0){ qx }else{ n*mx / (1+(n-ax)*mx) } 
     qx[nmax] <- 1
     
     px       <- 1-qx
     lx       <- c(1,cumprod(1-qx))*lx0 
     lx       <- lx[1:nmax]
     dx       <- lx*qx
     Lx       <- n*lx - ax*dx
     Lx[nmax] <- ax[nmax]*dx[nmax]
     Lx[is.na(Lx)] <- 0
     Tx       <- rev(cumsum(rev(Lx)))
     # Tx[nmax] <- max(dx[nmax,], Lx[nmax])
     ex       <- Tx/Lx
     ex[is.na(ex)] <- 0
     ex[nmax] <- ax[nmax]
     
     lt <- data.frame( x=x, mx=round(mx,6), qx=round(qx,6), ax=ax, 
                       lx=round(lx), dx=round(dx), Lx=round(Lx), 
                       Tx=round(Tx), ex=round(ex,2))
     lt.exact <- data.frame( x=x, mx=mx, qx=qx, ax=ax, 
                             lx=lx, dx=dx, Lx=Lx, Tx=Tx, ex=ex)
     out <- list(lt=lt, lt.exact=lt.exact, process_date = date())
     return(out)
}



#' mx to qx
#'
#' This is a description
#' @keywords internal
mx_qx <- function(ux, x, out = 'qx'){
     nmax     <- length(x)
     n        <- rep(1,nmax)
     ax       <- n/2
     ax[1] <- ifelse(x[1] == 0, .1, .5)
     vect <- switch (out,
                     qx = ux / (1+(1-ax)*ux),
                     mx = ux/(n-ux*(n-ax))  )
     return(vect)
}
