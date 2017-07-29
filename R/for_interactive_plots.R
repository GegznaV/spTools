# 4 subplots (plotly) -------------------------------------------

# supbplots_4: 2 types of spectra in 4 (2x2) subplots

#' 4 subplots (plotly)
#'
#' supbplots_4: 2 types of spectra in 4 (2x2) subplots
#' @param sp__i
#' @param sp
#' @param gr
#'
#' @return
#' @export
#'
#' @examples
supbplots_4 <- function(sp__i,sp, gr = "HistGr"){
    g1 <- ggplot(sp__i, aes_string(color = gr)) +
        geom_line(alpha = .2) +
        geom_point(alpha = .5, size = .3) +
        ggtitle("Spectra: normalized")
    g2 <- ggplot(sp, aes_string(color = gr)) +
        geom_line(alpha = .2) +
        geom_point(alpha = .5, size = .3) +
        ggtitle("Spectra: derivatives")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    g01 <- qplot_spRange(sp__i, by = gr, palette = NULL) +
        ggtitle("Intensity range: normalized")
    g02 <- qplot_spRange(sp, by = gr, palette = NULL) +
        ggtitle("Intensity range: derivatives")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subplot_2(g1, g01, g2, g02)
}

# =============================================================================
#' sub_plot
#'
#' @param ...
#' @param shareX
#' @param shareY
#' @param titleX
#' @param titleY
#' @param title
#'
#' @return
#' @export
#'
#' @examples
#' qplot_spRange(chondro, "clusters") +
#'     layer_spStat(chondro,"clusters", mean, size = 1)
#'
sub_plot <-
    function(... ,
             shareX = T, shareY = T,
             titleX = T, titleY = T,
             title = ""){

        subplot(... ,
                shareX = shareX, shareY = shareY,
                titleX = titleX, titleY = titleY) %>%
            plotly_tidy0()  %>%
            plotly::layout(title = title)
    }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
