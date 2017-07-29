#' format_ratio
#'
#' @param santykis
#'
#' @export

format_ratio <- function(santykis){
    santykis %<>% round(2)
    santykis[santykis == 0]   <- NaN
    santykis[santykis == Inf] <- NaN

    return(santykis)
}