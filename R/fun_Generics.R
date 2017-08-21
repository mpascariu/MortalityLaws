# S Functions
# Classes and methods for 'MortalityLaw'

#' @keywords internal
#' @export
print.MortalityLaw <- function(x, ...) {
  cat(paste(as.matrix(x$info$model.info[, c(2, 3)]), collapse = ':\n'))
  cat('\n\nCoefficients:\n')
  digits <- if (all(coef(x) < 1e-3)) 8 else 5
  print(round(coef(x), digits))
}

#' @keywords internal
#' @export
summary.MortalityLaw <- function(object, ...) {
  cat(paste(as.matrix(object$info$model.info[, c(2, 3)]), collapse = ':\n'))
  cat('\n\nDeviance Residuals:\n')
  print(round(summary(as.vector(as.matrix(object$residuals))), 5))
  cat('\nCoefficients:\n')
  digits <- if (all(coef(object) < 1e-3)) 8 else 5
  print(round(coef(object), digits ))
  
  cat('\nGoodness of fit:\n')
  print(round(object$goodness.of.fit, 2))
}

#' @keywords internal
#' @export
plot.MortalityLaw <- function(x, ...){
  with(x$input, if (is.matrix.or.data.frame(mx, qx, Dx, Ex)) {
         stop('Plot function not available for this type of object.', call. = FALSE)})
  
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
    if (is.null(x$input$mx)) {
      y = x$input$Dx/x$input$Ex
    } else {
      y = x$input$mx
      }
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


# ---------------
#' @keywords internal
#' @export
print.ReadHMD <- function(x, ...){
  cat('Human Mortality Database (www.mortality.org)\n')
  cat('Downloaded by:', x$input$username, '\n')
  cat('Download Date:', x$download.date, '\n')
  cat('Type of data:', x$input$what, '\n')
  cat('Countries included:', x$input$countries, '\n\nData:\n')
  print(head_tail(x$data, hlength = 5, tlength = 5))
}

#' @keywords internal
#' @export
print.availableHMD <- function(x, ...) {
  cat('Human Mortality Database (www.mortality.org)\n')
  cat('Checked Date:', x$checked.date, '\n')
  cat('Number of countries/populations:', length(x$countries), '\n\n')
  cat('The list below contains the populations currently included in the HMD,\n')
  cat('with the range of years covered by the period life tables:\n\n')
  print(x$avalable.data)
}



#' @keywords internal
#' @export
print.availableLF <- function(x, ...) {
  cat("\nLoss functions available in the package:\n\n")
  print(x$table, right = F)
  cat("\nLEGEND:\n")
  cat(x$legend, sep = '\n')
  cat("\nHINT:\n")
  cat(x$hint, sep = '\n')
}

#' @keywords internal
#' @export
print.availableLaws <- function(x, ...) {
  cat("\nMortality laws available in the package:\n\n")
  print(x$table, right = F)
  cat("\nLEGEND:\n")
  print(x$legend, right = FALSE, row.names = FALSE)
}

