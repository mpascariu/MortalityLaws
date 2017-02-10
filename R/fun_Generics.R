# S Functions
# Classes and methods for 'MortalityLaw'

#' @keywords internal
#' @export
print.MortalityLaw <- function(x, ...) {
  cat('Model:\n')
  cat(x$model_info, '\n')
  cat('\nCoefficients:\n')
  print(round(x$coefficients, 5))
}

#' @keywords internal
#' @export
summary.MortalityLaw <- function(object, ...) {
  cat('Model:\n')
  cat(object$model_info, '\n')
  cat('\nCall:\n')
  print(object$call)
  cat('\nDeviance Residuals:\n')
  print(round(summary(as.vector(as.matrix(object$residuals))), 5))
  cat('\nCoefficients:\n')
  print(round(object$coefficients, 5))
  cat('\nLog-Likelihood = ', round(object$logLikelihood, 2),
      ' AIC = ', round(object$AIC, 2), 
      ' BIC = ', round(object$BIC, 2))
}

#' @keywords internal
#' @export
plot.MortalityLaw <- function(x, ...){
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  lay_mat <- matrix(c(1, 2, 3, 1, 2, 3), ncol = 3, byrow = TRUE)
  layout(lay_mat, widths = c(6, 6, 1.5), heights = c(1.5, 6, 6), respect = T)
  
  age  = x$input$x
  age2 = x$input$fit.this.x
  select.x = age %in% age2
  
  # ----- Plot 1 -----
  par(mar = c(5, 5, 4, 1), cex.lab = 1.8, cex.axis = 1.5)
  y = if (is.null(x$input$mx)) { x$input$Dx/x$input$Ex 
  }else{x$input$mx }
  fit_y = x$fitted.values
  pos_x <- quantile(age,  p = seq(0, 1, by = 0.25), type = 1)
  pos_y <- round(quantile(log(y),  p = seq(0, 1, by = 0.25), type = 1), 1)
  
  plot(age, y = log(y), pch = 16, cex = 2, cex.main = 1.8,
       ylab = 'log(mx)', xlab = 'Age (x)', axes = FALSE,
       main = 'Observed and Fitted \nAge-Specific Death Rate',
       panel.first = rect(min(age2),-1e6, max(age2), 1e6, 
                          col = 'grey97', border = F) ) 
  box(col = 'grey80'); axis(1, at = pos_x); axis(2, at = pos_y)
  lines(age, log(fit_y), col = 3, lwd = 3)
  
  legend('bottomright', bty = "n", legend = c("Observed", "Fitted"),
         lty = c(NA, 1), pch = c(16, NA),
         col = c(1, 3), cex = 1.3, lwd = 3)
  
  # ----- Plot 2 -----
  resid <- x$residuals[select.x]
  par(mar = c(5, 5, 4, 1), cex.lab = 1.8, cex.axis = 1.5)
  plot(age2, resid, pch = 16, cex = 2, 
       main = 'Residual plot', cex.main = 1.8,
       xlab = 'Age (x)', ylab = 'Error', axes = F)
  pos_x2 <- quantile(age2,  p = seq(0, 1, by = 0.25), type = 1)
  box(col = 'grey80'); axis(1, at = pos_x2); axis(2)
  abline(h = 0, lty = 2, col = 'grey80')
  
  # ----- Plot 3 -----
  xhist <- hist(resid, breaks = 8, plot = FALSE)
  par(mar = c(5, 0, 4, 1))
  barplot(xhist$counts, axes = T, space = 0, 
          horiz = TRUE, xlab = 'Frequency')
  par(def.par)  #- reset to default
}


# ---------------
#' @keywords internal
#' @export
print.ReadHMD <- function(x, ...){
  cat('Human Mortality Database (www.mortality.org)\n')
  cat('Downloaded by:', x$input$username, '\n')
  cat('Download Date:', x$download.date, '\n')
  cat('Type of data:', x$input$what, '\n')
  cat('Countries included:', x$input$countries, '\n\nData:\n')
  print(headTail(x$data, hlength = 8, tlength = 8))
}