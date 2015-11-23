#' Add Logo Utility
#'
#' @param x,y position coordinate x and y
#' @param h   height in unit of lines
#' @param weight_asp weight of aspect, default 1, means keeping original aspect
#' @return Invisible NULL
#' @export
plotlogo <- function(x, y, h, weight_asp = 1)
{
  logo <- system.file("pixmaps", "logo.ppm", package=getPackageName())
  p <- suppressWarnings(read.pnm(logo))  # read.pnm() open issue

  aspect <- p@size[2]/p@size[1]   # aspect ratio width/height
  y1 <- y
  y2 <- y1 + h
  x1 <- x
  x2 <- x1 + aspect*h

  pixmap::addlogo(p, px=c(x1, x2), py=c(y1, y2), asp=aspect)

  return(invisible(NULL))
}
