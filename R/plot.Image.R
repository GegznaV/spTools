# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!"EBImage"  %in% installed.packages()) {
    #' require("EBImage")
    source("http://bioconductor.org/biocLite.R")
    biocLite("EBImage")
}


# plot image of class `EBImabe` -------------------------------------------

#' Plot image of class `EBImabe`
#'
#' @param x
#' @param title
#' @param method
#' @param all
#' @param ...
#'
#' @return
#' @export
#'

plot.Image <- function(x,
                       title = deparse(substitute(x), width.cutoff = 500L, nlines = 1),
                       method = "raster",
                       all = FALSE,
                       ...){
    EBImage::display(x,
                     title = title,
                     method = method,
                     all = all,
                     ...)
}

