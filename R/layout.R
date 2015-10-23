#' Calcualte greatest common divisor of two integers
#' 
#' @param x, y Two integers
#' @return The greatest common divisor
#' @export
gcd <- function(x, y) {
  while (x != 0) {
    tmp <- x
    x <- y %% x
    y <- tmp
  }
  return(y)
}

#' Calcualte smallest common multiple of two integers
#' 
#' @param x, y Two integers
#' @return The smallest common multiple
#' @export
scm <- function(x, y) {
  return(x / gcd(x, y) * y)
}

#' Calcualte the layout matrix
#' 
#' @param v a vector each element indicating the total number of plots in one row
#' @param equal a logical value indicates whether the layout should be equally distributed
#' @param ratio if equal is FALSE, supply division ratio
#' @return A matrix which can be used as graphics::layout argument
#' @export
layoutmat <- function(v=1, equal=TRUE, ratio=equal)
## TODO: add non-equally distributed condition
{
  stopifnot(is.numeric(v))
  stopifnot(all(v>0))
  v <- round(v)

  if(length(v) == 1)
    return(matrix(1:v, nrow=1, byrow=T))

  ncol <- 1
  for (i in 1:length(v))
    ncol <- scm(ncol, v[i])

  cum <- cumsum(v)
  nrep <- ncol/v
  x <- unlist(mapply(rep, c(0, cum[-length(cum)]), v))
  y <- unlist(sapply(v, function(x) 1:x))
  z <- unlist(mapply(rep, nrep, v))

  vmat <- unlist(mapply(function(x,y,z) rep(x+y,z), x, y, z))
  return(matrix(vmat, nrow=length(v), byrow=T))
}













