# ============================================================
#' gg_perf
#'
#' @param sp_compared
#'
#' @return
#' @export
#'
#' @examples
#'
gg_perf <- function(sp_compared){
    spc <- sp_compared[[]]

    # 20% of best AUC values between 0.5 and max(AUC)
    max_val <- max(spc)
    best_20 <- .5 + (max_val - .5) * .8

    # sp_best20 <- sp_compared[,,spc < best_20]
    sp_best20 <- sp_compared
    sp_best20[[spc < best_20]] <- NA

    # Plot
    ggplot(sp_compared, aes(color = Compared)) +
        geom_hline(yintercept = best_20,
                   color = "grey2",
                   linetype = "dashed") +
        annotate("text",
                 x = (
                     min(wl(sp_compared)) +
                         diff(range(wl(sp_compared)))*c(0.12,.5,0.9)
                 ),
                 y = best_20*1.02,
                 vjust = 0,
                 label = "Max 20%", size = 2.5) +
        set_ggLims(c(.5,1),"y") +
        geom_line(size = 1) +
        # geom_point() +
        geom_point(data = ldf(sp_best20), aes(fill = spc),
                   colour = "black",
                   pch = 21,
                   size = 2) +
        scale_color_brewer(palette = "Accent") +
        scale_fill_gradientn(name = "Max. 20% of\n" %++% sp_compared@label$spc,
                             colours = c("tomato4","tomato3","orange3",
                                         "skyblue1", "skyblue3", "olivedrab", "green4"),
                             breaks = seq(.5,1,.25),
                             limits = c(0.45,1.01)) +
        guides(fill = guide_colorbar(direction = "horizontal",
                                     title.position = "top",
                                     order = 2),
               colour = guide_legend(order = 1)) +
        ggtitle("Performance")
}
# gg_perf(perf_SeSp)

# =============================================================================
#' Quality of fit
#'
#' @param loadings
#' @param scores
#' @param spectra
#'
#' @return
#' @export
#'
#' @examples
#'
QOF <- function(loadings, scores, spectra = Spectra){
    fit <- reconstructSp(loadings, scores)
    evaluation <- quality_of_fit(spectra, fit)


    ggplot(stack(evaluation), aes(values, col = ind, fill = ind)) +
        geom_density(alpha = .2) +
        facet_wrap(~ind, scales = "free", ncol = 1, strip.position = "right" ) +
        labs(fill = NULL, color = NULL)

    # op <- par(mfrow = c(3,1))
    #     plot(density(evaluation$RMSE), main = "RMSE", col = 2)
    #     plot(density(evaluation$LOF),  main = "Lack-of-fit")
    #     plot(density(evaluation$GOF),  main = "Goodness-of-fit")
    # par(op)
}


# =============================================================================
#' sub_plot_range
#'
#' @param obj
#' @rdname sub_plot
#' @return
#' @export
#'
#' @examples
sub_plot_range <- function(obj){
    sub_plot(
        qplot_spRange(obj$train, "Compared") +
            ggtitle("Training") +
            set_ggLims(c(0.1, 1)),

        qplot_spDistrib(obj$test, "Compared") +
            ggtitle("Testing") +
            set_ggLims(c(0.1, 1)),

        title = "Distributions of classification performance"
    )
}

# =============================================================================
#' sub_plot_means
#'
#' @param obj
#' @rdname sub_plot
#' @return
#' @export
#'
#' @examples
sub_plot_means <- function(obj){
    sub_plot(
        qplot_spStat(obj$train,"Compared", mean) +
            ggtitle("Training") +
            set_ggLims(c(0.2, 1)),

        qplot_spStat(obj$test,    "Compared", mean) +
            ggtitle("Testing") +
            set_ggLims(c(0.2, 1)),

        title = "Means of classification performance"
    )
}

# =============================================================================
# Barplot -----------------------------------------

#' @name barplots_
#' @title barplots_
#'
#' @param Spectra
#' @param gr
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
#'
barplots_ <- function(Spectra, gr = "HistGr", ID = "ID", alpha = 1,
                      title_id = "Specimens", tilte_sp = "Spectra"){

    Spectra$ID <- getVarValues(ID, Spectra)
    bp1 <- gg_barplot(Spectra, gr, alpha = .5) +
        labs(y = "Number of spectra", title = tilte_sp) + x30()
    LIMS <- c(ggLims(bp1, "x"), 0, ggLims(bp1)[2])
    bp1 <- bp1 + ggLims(LIMS)

    bp2 <-
        gg_barplot(Spectra[!duplicated(Spectra$ID)], gr, alpha = 1) +
        labs(y = "Number of specimens", title = title_id) +
        ggLims(LIMS, expand = FALSE) +
        x30()

    # gg <- gridExtra::arrangeGrob(bp1, bp2, nrow = 1)

    # gg <- subplot(bp1, bp2, nrow = 1, shareY = TRUE)  %>%
        # plotly_tidy()

    gg <- cowplot::plot_grid(bp2, bp1,
                             nrow = 1,
                             align = "h")

    gg
}

#' @rdname barplots_
#' @export
gg_barplot <- function(Spectra, gr = "HistGr", alpha = 1){
    ggplot(Spectra$.., aes_string(gr)) +
        geom_bar(stat = "count",
                 aes_string(fill = gr),
                 color = 1,
                 alpha = alpha) +
        geom_text(stat = 'count', aes(label = ..count..), vjust = -.2) +
        x30() +
        labs(x = labels(Spectra, gr)) +
        scale_x_discrete(drop = FALSE) +
        scale_fill_discrete(drop = FALSE)
}

# =============================================================================
#' sub_plot_range_and_means
#'
#' @param obj a list of hyperSpec objects
#' @param percent percentage of y axis range to be plotted
#' @param size size of line for means
#' @param limsY limits for y axis
#'
#' @return
#' @export
#'
#' @examples
sub_plot_range_and_means <- function(obj,
                                 percent = 95,
                                 size  = .8,
                                 limsY = c(0, 1),
                                 return_as = c("plotly","ggplot"))
 {
    TRAIN <- obj$train_performance
    TEST  <- obj$test_performance

    # TEST[ , c("type", "Compared", "Compared_1", "Compared_2", "Fold", "spc")]

    ggplot_fun <- ggplot_mean_and_range

    bp1 <- ggplot_fun(TRAIN) + ggtitle("Training")
    bp2 <- ggplot_fun(TEST)  + ggtitle("Testing")

    # Return either as `ggplot` or as `plotly`
    Title <- "Means and " %++% percent %++%
        "% range of classification performance values"

    return_as <- match.arg(return_as)
    switch(return_as,
           "plotly" = sub_plot(rmExpr(bp1), rmExpr(bp2), title = Title),
           "ggplot" = gridExtra::arrangeGrob(bp1, bp2, nrow = 1)
           )
}
# =============================================================================

#' @rdname cutoffs_plot
#' @export
plot_cutoffs <- function(...){
    cutoffs_plot(...)
    }


#' sp_plot_range_and_cutoffs
#'
#' @param obj a list of hyperSpec objects
#' @param percent percentage of y axis range to be plotted
#' @param size size of line for means
#' @param limsY limits for y axis
#'
#' @name cutoffs_plot
#' @export
#'
#' @examples
#' cutoffs_plot(obj)
#'
cutoffs_plot <- function(obj,
                                 percent = 95,
                                 # size = .8,
                                 return_as = c("plotly","ggplot", "list"))
 {

    if (!inherits(obj, "sp_class_perform_cv"))
        stop("The class of the `obj` must be'sp_class_perform_cv'")

    # Skaičiavimai:

    return_as <- match.arg(return_as)

    # Reikiamų stulpelių pavadinimai
    columns <- c("type", "Compared", "Compared_1", "Compared_2", "Fold", "spc")

    # Pasirenkam duomenis (spektrus)
    DATA <- obj$data

    GR_NAME <- obj$obj[[1]]$compared_by_var
    GR <- DATA[[, GR_NAME, drop = TRUE]]

    # Pasirenkam slenkstines reikšmes
    CUTOFFS      <- obj$cutoffs
    groupings    <- unique(CUTOFFS$Compared)
    CUTOFFS_list <- hyperSpec::split(CUTOFFS, CUTOFFS$Compared)

    figs <- knitrContainer::knitrContainer()
    # Return either as `ggplot` or as `plotly`


    # Analizę atliekame kievienai lygintų grupių porai.
    for (i in seq_along(groupings)) {

        Title <- groupings[i]  %++% ": Data and Cutoff Values"

        grouping <-
            strsplit(groupings[i], split = " vs. ", fixed = TRUE)  %>%
            Reduce(rbind, .)

       inds <- GR %in% grouping

       DATA$type <- DATA[[, GR_NAME, drop = TRUE]]
       CUTOFF    <- CUTOFFS_list[[groupings[i]]][, columns]
       CUTOFF$type  %<>% as.factor()
       DF1 <- DATA[inds, c("type", "spc")]
       DF2 <- CUTOFF[, c("type", "spc")]

       cp1 <- cutoffs_plot_df(cutoffs_get_df(DF1, DF2, 95, centering = F)) +
                    ggtitle("Range of intensities: 95%") +
           ylab("(Range: 95%)")

       # cp2 <- cutoffs_plot_df(cutoffs_get_df(DF1, DF2, 95, centering = T)) +
       #              ggtitle("Range of intensities: 95%") +
       #     ylab("Centered (95%)")

       cp3 <-
           cutoffs_plot_df(cutoffs_get_df(DF1, DF2, 50, scaling = T)) +
               ggtitle("Range of intensities: 50% (autoscaled)") +
               ylab("Scaled (50%)")



       figs  %<>%  add_as_is(switch(
           return_as[1],
           "plotly" = sub_plot(cp1,  cp3, title = Title, nrows = 2),
           # "ggplot" = gridExtra::arrangeGrob(cp1,  cp3, nrow = 2)
           "ggplot" = cowplot::plot_grid(cp1,  cp3, nrow = 2),
           "list"   = list(plot1 = cp1, plot2 = cp3)
       ))
    }
    # Rezultatas (grafikai "konteineryje")
    figs
}

# =============================================================================
# Apibrėžiamos pagalbinės funkcijos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name cutoffs_plot
#' @export
cutoffs_calcultate_stats <- function(sp, percent = 100, center = FALSE, scale = FALSE) {
    sp <- hyperSpec::scale(sp, center = center, scale = scale)

    spDF <- spStat_ldf(
        sp,
        sp$type,
        FUN = quantile,
        probs = percent2probs(percent),
        var_names = c("min", "max")
    )

    spDF$mean <- spStat_ldf(sp,
                            sp$type,
                            FUN = mean)$mean

    spDF
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name cutoffs_plot
#' @export
cutoffs_get_df <- function(data,
                           cutoffs,
                           percent = 100,
                           scaling = FALSE,
                           centering = scaling,
                           percent.cutoffs = 95 # "conf.level" for CI of cut-offs
) {

    if (scaling == TRUE) {
        data_sd <- hyperSpec::apply(data, 2, "sd")
    } else {
        data_sd <- FALSE
    }

    if (centering == TRUE) {
        data_mean <- hyperSpec::apply(data, 2, "mean")
    } else {
        data_mean <- FALSE
    }

    rbind(
        cutoffs_calcultate_stats(data,    percent,        data_mean, data_sd),
        cutoffs_calcultate_stats(cutoffs, percent.cutoffs,data_mean, data_sd)
    )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @param df a data frame
#' @param lwd_mean width of line for mean
#' @param alpha transparency
#' @param COLORS vector with either names or codes of 3 colors
#'
#' @name cutoffs_plot
#' @export
cutoffs_plot_df <- function(df,
                            lwd_mean = 1,
                            alpha = 0.3,
                            COLORS = c("#e41a1c", "#377eb8", "black")) {
    ggplot(df,
           aes_string(
               x  = ".wavelength",
               y  = "mean",
               ymin  = "min",
               ymax  = "max",
               color = "type",
               fill  = "type"
           )
    ) +
        geom_ribbon(alpha = alpha) +
        geom_line(lwd = lwd_mean) +
        labs(x = labels(df)$.wavelength,
             y = labels(df)$spc
             , color = "Groups"
             , fill = "Groups"

             # , title = Title
        ) +

        scale_color_manual(values = COLORS ) +
        scale_fill_manual(values = COLORS)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# =============================================================================




#' ggplot_mean_and_range
#'
#' @param x
#' @param by
#' @param percent
#'
#' @return
#' @export
#'
#' @examples
ggplot_mean_and_range <- function(x, by = "Compared", percent = 95, limsY = c(0, 1)){
    # qplot_spRange(   x, by =  "Compared", percent = percent) +
    #     layer_spStat(x, by =  "Compared", mean,  size = size) +
    #     set_ggLims(limsY) +
    #     geom_hline(yintercept = 0.5, lty = 2, col = "grey30")

    qplot_spRangeMean(x, by =  by, percent = percent) +
        set_ggLims(limsY) +
        geom_hline(yintercept = 0.5, lty = 2, col = "grey30")

}


#' sub_plot_range_and_means
#'
#' @param obj a list of hyperSpec objects
#' @param percent percentage of y axis range to be plotted
#' @param size size of line for means
#' @param limsY limits for y axis
#'
#' @return
#' @export
#'
#' @examples
sub_plot_cutfoos <- function(obj,
                                     percent = 95,
                                     size = .8,
                                     limsY = c(0, 1),
                                     return_as = c("plotly","ggplot"))
{
    CUTOFFS <- collapse(obj$cutoffs)

    ggplot_mean_and_range <- function(x){
        qplot_spRange(   x, by =  "Compared", percent = percent) +
            layer_spStat(x, by =  "Compared", mean,  size = size)
    }

   ggplot_mean_and_range(CUTOFFS) + ggtitle("Cut-offs")

}

