# -------------------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last Update: Thu Jul 20 22:06:31 2023
# -------------------------------------------------------------- #


#' Summary function - display head and tail in a single data.frame
#' The original code for this function was first written for 'psych' R package
#' here we have modified it a bit
#' @param x A matrix or data frame or free text
#' @param hlength The number of lines at the beginning to show
#' @param tlength The number of lines at the end to show
#' @param digits Round off the data to digits
#' @param ellipsis separate the head and tail with dots
#' @return Print table's head and tail
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


#' Extracting the last n characters from a string
#' @param x a string
#' @param n number of characters
#' @return A character vector of length n
#' @keywords internal
substrRight <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}
