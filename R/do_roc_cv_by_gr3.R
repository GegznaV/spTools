#' # =============================================================================
#'
#' #' do_ROC_CV_by_gr
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' do_ROC_CV_by_gr <- function() {
#'     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'     # Spectra <- readRDS("D:/Dokumentai/R/Spektroskopija/PAP_RK_2015/Data/2-tmp-spectra.RDS")
#'     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#'     container <- knitrContainer()
#'
#'     Spectra <- Spectra[, , min ~ 700]
#'
#'     k_folds = 3
#'     times   = 10
#'
#'     variables_to_analyze <-
#'         c(
#'             # "Batch",
#'
#'              "CitoGr"
#'             # , "Hybrid_normal"
#'             # , "Hybrid_cervicitis"
#'             # , "Hybrid_cin3_cis"
#'
#'             # ,"HistGr"
#'             # ,"HistGr1", "HistGr2"
#'
#'             # , "HPV_16", "HPV_hr"
#'             , "p16_Ki67"
#'             # , "age_gr",     "Sediment_size2"
#'         )
#'
#'     # Var <- "p16_Ki67"
#'     # i <- 1
#'
#'     for (i in seq_along(variables_to_analyze)) {
#'
#'         # for(i in 1) {
#'         Var <- variables_to_analyze[i]
#'
#'         # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'         # message(sprintf("Cycle: % 2g\t Variable: %s",i, Var))
#'         # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'         # nID_nSp(Spectra, "ID", Var)
#'
#'         Spectra <- hyRm_palette(Spectra)
#'
#'         gg1 <- barplots_(Spectra, gr = Var)
#'
#'         gg2    <- qplot_spRange(Spectra, Var, percent = 95)
#'         lims_2 <- ggLims(gg2)
#'
#'         gg3 <- qplot_spRange(Spectra, Var, percent = 50) +
#'             ggLims(lims_2, expand = FALSE)
#'
#'         gg4 <- qplot_spStat(Spectra, Var, median) +
#'             ggLims(lims_2, expand = FALSE)
#'
#'         gg5 <- qplot_spStat(Spectra, Var, mad, "Median absolute deviation")+
#'             ggLims(lims_2, expand = FALSE)
#'
#'         meanTr <- function(x) mean(x, trim = .5)
#'         CSCb <- center_subtracted_centers(Spectra, Var, meanTr, balanced = T)
#'         p2_BCSC <- qplot_sp(CSCb, names.in = Var) +
#'             ggtitle("Balanced mean subtracted group means") +
#'             ggLims(lims_2[2] * c(-1, 1)/4, expand = FALSE)
#'
#'
#'         cont4 <- tryCatch(
#'             (rez <- ROC_with_cv(Spectra,
#'                                 Var = Var,
#'                                 k = k_folds,
#'                                 times = times,
#'                                 range_percent = 95)
#'              )$plot,
#'
#'             error   = function(x) '**Grafiko "Se-Sp plot" nebuvo galima nubraižyti**',
#'             finally = function(x) '**Grafiko "Se-Sp plot" nebuvo galima nubraižyti**'
#'         )
#'         # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#'         container %<>% add_as_heading(labels(Spectra, Var) %++% " (" %++% Var %++% ")")
#'         container %<>%
#'             add_as_is(gg1) %>%
#'             add_as_is(ggplotly_tidy(gg4))      %>%
#'             add_as_is(ggplotly_tidy(gg5))      %>%
#'             add_as_is(ggplotly_tidy(gg2))      %>%
#'             add_as_is(ggplotly_tidy(gg3))      %>%
#'             add_as_is(ggplotly_tidy(p2_BCSC))  %>%
#'             add_as_heading2("ROC analysis" )   %>%
#'             add_as_text("ROC analysis with **"     %++%
#'                             times %++% " times** repeated **" %++%
#'                             k_folds %++% "-fold** cross-vaidation.")
#'
#'         container %<>% Join(as.knitrContainer(cont4))
#'
#'         rm(gg1, gg2, gg3, gg4, gg5, p2_BCSC, cont4)
#'         gc()
#'     }
#'     container
#' }





