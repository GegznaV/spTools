# color_selector -----------------------------------------------------------------

#' color_selector
#'
#' @param Var
#'
#' @return
#' @export
#'
#' @examples
#'
color_selector <- function(Var="NULL"){
    require(spHelper)
    # Select colors
    Colors <- switch(Var,
                     "CitoGr" = c("Forestgreen","Orange","#E41A1C"),
                     "HistGr" = palette_PAP,
                     "age_gr" = c("green3", "tan3"),
                     "is_tooLow_I"          = c("#0080ff","tomato2"),
                     "status_outlier_IQR"   = c("#0080ff","tomato2"),
                     "status_outlier_nSD"   = c("#0080ff","tomato2"),
                     "status_outlier_PCout" = c("tomato2", "#0080ff"),
                     # Otherwise:
                     c("#0080ff","orange", RColorBrewer::brewer.pal(9, "Set3"))
    )
    return(Colors)
}