gcd <- function(x, y) {
  while (x != 0) {
    tmp <- x
    x <- y %% x
    y <- tmp
  }
  return(y)
}

scm <- function(x, y) {
  return(x / gcd(x, y) * y)
}

layoutmat <- function(v, equal=TRUE, ratio=equal)
## TODO: add non-equally distributed condition
{
  stopifnot(is.numeric(v))
  stopifnot(any(v>0))
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













