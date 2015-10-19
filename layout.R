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

layoutmat <- function(v, equal=TRUE, ratio=ifelse(equal, rep(1, length(v)), ratio))
{
  ncol <- 1
  for (i in 1:length(v))
    ncol <- scm(ncol, v[i])

  v <- as.list(v)

  sapply(v, function(x) rep(x, 30/x))
}

