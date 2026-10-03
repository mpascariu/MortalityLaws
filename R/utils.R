# -------------------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last Update: Thu Jul 20 22:06:31 2023
# -------------------------------------------------------------- #


#' Display the Head and the Tail of an Object in a Single Data Frame
#'
#' Internal printing helper showing only the first and the last rows of a
#' matrix, a data frame or free text. The code was originally written for the
#' \pkg{psych} package and is modified here.
#'
#' @param x A matrix, a data frame or free text.
#' @param hlength The number of lines at the beginning to show.
#' @param tlength The number of lines at the end to show.
#' @param digits Round off the numeric columns to this number of digits.
#' @param ellipsis Separate the head and the tail with dots.
#' @return A data frame, or a character string for free text, with the head
#' and the tail of \code{x}.
#' @noRd
head_tail <- function(x,
                      hlength  = 4,
                      tlength  = 4,
                      digits   = 4,
                      ellipsis = TRUE) {

  if (is.data.frame(x) | is.matrix(x)) {

    if (is.matrix(x)) {
      x <- data.frame(unclass(x))
    }

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

    h   <- head(x, hlength)
    t   <- tail(x, tlength)
    out <- paste(paste(h, collapse = " "), "...   ...",
                 paste(t, collapse = " "))
  }

  return(out)
}


#' Extract the Last n Characters from a String
#'
#' Internal helper used to build short codes and labels out of longer strings.
#'
#' @param x A character vector.
#' @param n The number of characters to extract, counted from the right.
#' @return A character vector with the last \code{n} characters of \code{x}.
#' @noRd
substr_right <- function(x, n) {

  out <- substr(x, nchar(x) - n + 1, nchar(x))
  return(out)
}
