# =============================================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' #' @rdname barplots_
#' #' @export
#' #'
#' barplots_1 <- function(Spectra, gr = "HistGr", alpha = 1){
#'
#'     bp2 <- barplot_(Spectra[!duplicated(Spectra$ID)], gr, .5) +
#'         labs(y = "Number of specimens", title = "Samples")
#'     bp2
#' }
# =============================================================================
#' #' hist_
#' #'
#' #' @param fill
#' #' @param bins
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #'
#' hist_ <- function(fill = "grey", bins = 80) {
#'     geom_histogram(bins = bins, color = "black", fill = fill)
#' }
#'

# =============================================================================
#' #' # Compare zones (spectral ranges) -----------------------------------------------------------
#'
#' #' compare_sp_zones
#' #'
#' #' @param sp
#' #' @param zone
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' compare_sp_zones <- function(sp, zone) {
#'     data <- as.vector(sp$spc)
#'     df   <- data.frame(I = data, gr = sp$gr)
#'
#'     names(df) <- c(sprintf("Intensity at %s", zone), "Group")
#'
#'     xMedian <- eval_("stats::aggregate(`" %++% names(df)[1] %++% "` ~ `Group`, data=df, median)")
#'     xMedian$type <- "Median"
#'
#'     palet <- sp %>% hyGet_palette0()
#'
#'     p <- ggplot(df, aes_string(x=as.name(names(df)[1]),
#'                                color = "Group",
#'                                fill  = "Group")) +
#'         geom_density( alpha = .2) +
#'         geom_vline(data = xMedian,
#'                    aes_string(xintercept = as.name(names(df)[1]),
#'                               color = "Group",
#'                               linetype = "type"),
#'                    show.legend = TRUE) +
#'         scale_linetype_manual(name = "Statistic",values = "dashed") +
#'         scale_color_manual(breaks = palet$labels, values = palet$colors) +
#'         scale_fill_manual( breaks = palet$labels, values = palet$colors)
#'     print(p)
#'
#'
#'     # cat("\n")
#'     # rez <- wilcox.test(as.name(names(df)[1]) ~ `Group`, data=df)
#'     eval_("rez <- (kruskal.test(`" %++% names(df)[1] %++% "` ~ `Group`, data=df))")
#'
#'     print(rez)
#'
#'
#'     cat("\n")
#'     tbl <- eval_("ROC_(df$`" %++% names(df)[1]  %++% "`, df$Group, levels(df$Group))")
#'     cat("  \n  ")
#'     # set.alignment('center', row.names = 'center')
#'
#'     tbl <- Reduce(function(...) cbind(...), tbl)
#'
#'     cat("Performance measures at optimal cutoff point.\n\n")
#'     print(tbl)
#'
#'     # pander::pander(tbl, style = 'rmarkdown', justify = "center",
#'     #                caption ="Performance measures at optimal cutoff point.")
#'     cat("  \n  ")
#'
#' }
#'
#'

#' # =============================================================================
#' # Highlight zones in ggplot2 plot =============================================
#' #
#' # zones - #
#' # col - vector of colors to be used for each zone
#' #
#' # examples
#' #
#'
#' #' Highlight wavelength zones in ggplot plot of hyperSpec object
#' #'
#' #' @param p ggplot2 object
#' #' @param zones formula or list of formulas, that indicate which  zones of
#' #' wavelength axis should be highlighted e.g., 460~800.
#' #' @param col vector of colors to be used for each zone
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' zones <- c(400~460, 545~670)
#' #'
#' highlightZones <-
#'     function(p, zones , col = RColorBrewer::brewer.pal(9, "Set1")[1:length(zones)])
#'     {
#'         eval_("p + " %++% paste0("spZone(", zones, ",'", col, "')", collapse = " + "))
#'     }


# =============================================================================


# # ROC_ -------------------------------------------------------------
# ROC_ <-function(x,labels,label.ordering=NULL, make_plots = TRUE){
#     pred <- prediction0(x,labels,label.ordering)
#     if (make_plots==TRUE) ROC_plots(pred)
#     # pander::pander(ROC_table(pred),
#     #                caption ="Performance measures at optimal cutoff point.")
# }


# # # Plotly
# # # saveRDS(gg, "Example multi gg object.RDS")
# gg <- readRDS("Example multi gg object.RDS")
#
# library(spHelper)
# obj <-lapply(gg$age_gr$unpaired, ggplotly)  %>%
#     subplot(margin=.02 ,nrows=3,shareX=T,shareY=F,titleX=T,titleY=T) %>%
#     layout(title = "Spectra",  showlegend = TRUE) %>% plotly_tidy
#     # label_expr2text %>% plotly_modify_legend %>% plotly_modify_hover %>% plotly_annotation_rm
# obj %<>% plotly_build  %T>% str
# obj
#
# c(obj$data[[i]]$name, obj$data[[i]]$legendgroup)
#
# obj2 <-lapply(gg$age_gr$paired, ggplotly)  %>%
#     subplot(margin=.02, nrows=4, shareX=T, shareY=T, titleX=T, titleY=T) %>%
#     layout(title = "Spectra",  showlegend = TRUE) %>% plotly_tidy
#     # label_expr2text %>% plotly_modify_legend %>% plotly_annotation_rm
# obj2 %<>% plotly_build  %T>% str
# obj2
# obj2$data
#