
#' Plot Function for MortalityLaw
#' @param x an object of class MortalityLaw
#' @param ... Arguments to be passed to methods, such as graphical 
#' parameters (see \code{\link{par}}).
#' @keywords internal
#' @export
plot.MortalityLaw <- function(x, ...){
  with(x$input, if (find.my.case(Dx, Ex, mx, qx)$iclass != "numeric") {
    stop("Plot function not available for multiple mortality curves", 
         call. = FALSE)})
  
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  lay_mat <- matrix(c(1, 2, 3, 1, 2, 3), ncol = 3, byrow = TRUE)
  layout(lay_mat, widths = 1.5*c(6, 6, 1.5), 
         heights = 1.5*c(1.5, 6, 6), respect = T)
  
  age  = x$input$x
  age2 = x$input$fit.this.x
  select.x = age %in% age2
  
  # ----- Plot 1 -----
  par(mar = c(5, 5, 4, 1), cex.lab = 1.1, cex.axis = 1.3)
  
  if (!is.null(x$input$qx)) {
    y = x$input$qx 
  } else {
    y = with(x$input, if (is.null(mx)) Dx/Ex else mx)
  }  
  
  fit_y = x$fitted.values
  pos_x <- quantile(age,  p = seq(0, 1, by = 0.25), type = 1, na.rm = TRUE)
  pos_y <- round(quantile(log(y),  p = seq(0, 1, by = 0.25), 
                          type = 1, na.rm = TRUE), 1)
  
  plot(age, y = log(y), pch = 16, cex.main = 1.1,
       ylab = 'Mortality Level', xlab = 'x', axes = FALSE,
       main = 'Observed vs. Fitted',
       panel.first = rect(min(age2),-1e6, max(age2), 1e6, 
                          col = 'grey95', border = F) ) 
  box(col = 'grey80'); axis(1, at = pos_x); axis(2, at = pos_y)
  lines(age, log(fit_y), col = 3, lwd = 1.5)
  
  legend('bottomright', bty = "n", legend = c("Observed", "Fitted"),
         lty = c(NA, 1), pch = c(16, NA),
         col = c(1, 3), cex = 1.3, lwd = 3)
  
  # ----- Plot 2 -----
  resid <- x$residuals[select.x]
  par(mar = c(5, 5, 4, 1), cex.lab = 1.1, cex.axis = 1.3)
  plot(age2, resid, pch = 16,
       main = 'Residual plot', cex.main = 1.1,
       xlab = 'x', ylab = '', axes = F)
  pos_x2 <- quantile(age2,  p = seq(0, 1, by = 0.25), type = 1)
  box(col = 'grey80'); axis(1, at = pos_x2); axis(2)
  abline(h = 0, lty = 2, col = 'grey80')
  
  # ----- Plot 3 -----
  xhist <- hist(resid, breaks = max(10, round(length(resid)/5)), plot = FALSE)
  par(mar = c(5, 0, 4, 1), cex.lab = 0.9, cex.axis = 1.3)
  barplot(xhist$counts, axes = T, space = 0, 
          horiz = TRUE, xlab = 'Frequency')
  par(def.par)  #- reset to default
}


#' Summary function - display head and tail in a single data.frame
#' The original code for this function was first written for 'psych' R package
#' here we have modified it a bit
#' @param x A matrix or data frame or free text
#' @param hlength The number of lines at the beginning to show
#' @param tlength The number of lines at the end to show
#' @param digits Round off the data to digits
#' @param ellipsis separate the head and tail with dots
#' @keywords internal
head_tail <- function(x, hlength = 4, tlength = 4, digits = 4, ellipsis = TRUE){
  if (is.data.frame(x) | is.matrix(x)) {
    if (is.matrix(x)) x = data.frame(unclass(x))
    nvar <- dim(x)[2]
    dots <- rep("...", nvar)
    h    <- data.frame(head(x, hlength))
    t    <- data.frame(tail(x, tlength))
    for (i in 1:nvar) {
      if (is.numeric(h[1, i])) {
        h[i] <- round(h[i], digits)
        t[i] <- round(t[i], digits)
      } else {
        dots[i] <- NA
      }
    }
    out <- if (ellipsis) rbind(h, ... = dots, t) else rbind(h, t)
  } else {
    h <- head(x, hlength)
    t <- tail(x, tlength)
    out <- paste(paste(h, collapse = " "), "...   ...", paste(t, collapse = " "))
  }
  return(out)
}

