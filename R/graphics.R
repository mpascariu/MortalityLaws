
#' Plot Function for MortalityLaw
#' @param x An object of class MortalityLaw
#' @param ... Arguments to be passed to methods, such as graphical 
#' parameters (see \code{\link{par}}).
#' @seealso \code{\link{MortalityLaw}}
#' @examples 
#' # See complete example in MortalityLaw help page
#' @export
plot.MortalityLaw <- function(x, ...){
  with(x$input, if (find.my.case(Dx, Ex, mx, qx)$iclass != "numeric") {
    stop("Plot function not available for multiple mortality curves", 
         call. = FALSE)})
  
  age      <- x$input$x
  age2     <- x$input$fit.this.x
  select.x <- age %in% age2
  law      <- x$input$law
  if (law == "custom.law") {
    lawN <- "Custom Mortality"
  } else {
    lawN = unlist(availableLaws(law)$table['NAME'])
  }
  
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  lay_mat <- matrix(c(1, 2, 3, 1, 2, 3), ncol = 3, byrow = TRUE)
  layout(lay_mat, widths = 1.5*c(6, 6, 1.5), 
         heights = 1.5*c(1.5, 6, 6), respect = T)
  
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
       main = paste('Observed vs. Fitted', lawN, 'law'),
       panel.first = rect(min(age2),-1e6, max(age2), 1e6, 
                          col = 'grey95', border = F) ) 
  box(col = 'grey80'); axis(1, at = pos_x); axis(2, at = pos_y)
  lines(age, log(fit_y), col = 3, lwd = 1.5)
  
  legend('bottomright', bty = "n", legend = c("Observed", paste("Fitted", law)),
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



