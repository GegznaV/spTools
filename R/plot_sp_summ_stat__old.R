#
# # Plot many summary plots of spectra ------------------------------------------
# plot_sp_summ_stat <- function(sp, vars, zones)
# {
#     N <- length(vars)
#     gg <- list()
#
#     for (i in 1:N) {
#
#         Var <- vars[i]
#         # Header of iteration
#         cat(labels(sp, Var), sep = "\n")
#
#         # Assing variable to "gr"
#         sp$gr <- sp[[,Var,drop = TRUE]] %>% as.factor() %>% droplevels()
#
#         # # Statistical summary - NEVEIKIA
#         # pander::pander(nID_nSp(data = sp, ID = "ID", gr = Var))
#
#         # Select colors
#         Colors <- color_selector(Var)
#
#         sp <- hyAdd_color(sp, by = "gr", Colors)
#
#         # Plot all spectra =============================================================
#
#         sp_all_distribution <- {qplot_sp(sp, by = "gr") +
#                 ggtitle(labels(sp,Var)  %++% ": All Spectra")}
#
#
#         # ==============================================================================
#         # Distributions by groups ======================================================
#         # ==============================================================================
#
#         band100 <- qplot_spDistrib(sp,"gr", percent = 100)
#         yLIM <- band100 %>% get_ggLims("y")
#
#         band100 <- (band100 + set_ggLims(yLIM,"y"))  %>% highlightZones(zones)
#         # band90  <- (qplot_spDistrib(sp,"gr", percent = 90)  + set_ggLims(yLIM,"y")) %>% highlightZones(zones)
#         band50  <- (qplot_spDistrib(sp,"gr", percent = 50)  +
#                         set_ggLims(yLIM,"y")) %>% highlightZones(zones)
#
#         # Median and MAD  ------------------------------------------------------------
#         ggMed <- (qplot_spStat(sp, "gr", median) + set_ggLims(yLIM,"y")) %>%
#             highlightZones(zones)
#         ggMAD <- (qplot_spStat(sp, "gr", mad, "Median absolute deviation") +
#                       set_ggLims(yLIM,"y")
#         ) %>% highlightZones(zones)
#         # ggIQR <- qplot_spStat(sp, "gr", IQR, "Interquartile Range (IQR)") +
#         # set_ggLims(yLIM,"y")
#
#         # Differences between group means  ====================================================
#         meanTr <- function(x) mean(x, trim = .5)
#
#         CSC <- center_subtracted_centers(sp, "gr", meanTr)
#         p1_CSC <- qplot_sp(CSC, names.in = "gr") %>% highlightZones(zones) +
#             ggtitle("Total mean subtracted \ngroup means")
#
#         CSCb <- center_subtracted_centers(sp, "gr", meanTr, balanced = T)
#         p2_BCSC <- qplot_sp(CSCb, names.in = "gr") %>% highlightZones(zones) +
#             ggtitle("Balanced mean subtracted \ngroup means")
#
#         # ==============================================================================
#         # Scaled data ==================================================================
#         # ==============================================================================
#
#
#         # Median and MAD  ------------------------------------------------------
#
#         MED <- apply(sp,2,median)
#         MAD <- apply(sp,2,mad)   # median absolute deviation
#
#         scSp <- scale(sp, center = MED, scale = MAD)
#
#         yLIM_sc <- c(-1,1)*3.2
#
#         # CSCs -----------------------------------------------------------------
#         CSCb_sc    <- center_subtracted_centers(scSp, "gr", meanTr, balanced = T)
#
#         suppressMessages({
#             p3_BCSC_sc     <-
#                 (qplot_sp(CSCb_sc, names.in = "gr") + set_ggLims(yLIM_sc,"y")
#                 ) %>% highlightZones(zones) +
#                 ggtitle("Balanced mean subtracted \ngroup means (scaled data)")
#         })
#
#         band50_sc  <- (qplot_spDistrib(scSp, "gr", percent = 50) +
#                            set_ggLims(yLIM_sc,"y")) %>% highlightZones(zones) +
#             ggtitle("Distribution 50% arround \nmedian (scaled data)")
#
#
#
#         #  ------------------------------------------------------------------------
#
#
#
#
#         p_AUC <- ggplot(sp_compare_gr_wl(sp, "gr"), aes(color = Compared)) %>%
#             highlightZones(zones) + geom_line() + theme_bw() +
#             ggtitle("Spectrum of AUC") + set_ggLims(c(.5,1),"y")
#
#         p_SeSp <- ggplot(sp_compare_gr_wl(sp, "gr",measure = "sesp"),
#                          aes(color = Compared)) %>% highlightZones(zones) +
#             geom_line() + theme_bw() + set_ggLims(c(.5,1),"y") +
#             ggtitle("Spectrum of (Se+Sp)/2")
#         #
#         # p_AUC_sc <- ggplot(sp_compare_gr_wl(scSp, "gr"), aes(color = Compared))%>% highlightZones(zones) +
#         #     geom_line() + theme_bw() + ggtitle("Compare groups (AUC)\n(scaled data)")+ set_ggLims(c(.5,1),"y")
#         #
#         # p_SeSp_sc <- ggplot(sp_compare_gr_wl(scSp, "gr",measure = "sesp"), aes(color = Compared))%>% highlightZones(zones) +
#         #     geom_line() + theme_bw() + ggtitle("Compare groups (Se+Sp)/2\n(scaled data)")+ set_ggLims(c(.5,1),"y")
#
#
#         # Arrange ggplots  ----------------------------------------------------
#         # p <- list(paired_1 = list(ggMed,
#         #                band100,
#         #                p1_CSC,
#         #                p3_BCSC_sc
#         #                ),
#         #
#         #           paired_2 = list(ggMAD,
#         #                band50,
#         #                p2_BCSC,
#         #                band50_sc
#         #                ),
#         #
#         #           unpaired = list(sp_all_distribution,
#         #                           p_AUC,
#         #                           p_SeSp)
#         #           )
#
#
#         # print UNPAIRED plots ---------------------------------------------------------
#         # for (i in 1:length(p$unpaired)) {
#         # grid.newpage()
#         # grid.draw(p$unpaired[[i]])
#         #
#         # }
#
#         # print PAIRED plots ---------------------------------------------------------
#         # for (i in 1:length(p[[1]])) {
#         #     grid.newpage()
#         #     grid.draw(cbind(ggplotGrob(p$paired_1[[i]]),
#         #                     ggplotGrob(p$paired_2[[i]]),
#         #                     size = "last"))
#         # }
#
#
#         p <- list(paired = list(ggMed,      ggMAD,
#                                 band100,    band50,
#                                 p1_CSC,     p2_BCSC,
#                                 p3_BCSC_sc, band50_sc        ),
#
#
#                   unpaired = list(sp_all_distribution,
#                                   p_AUC,
#                                   p_SeSp)
#         )
#
#         gg[[Var]] <- p
#     }
#     invisible(gg)
# }
# # plot(sp,
# #      col = sp$.color,
# #      spc.nmax = nrow(sp))
# # title(main = subt("All Spectra",labels(sp,Var)))
# #
# # plot_hyPalette(sp, "gr", as.legend = TRUE, Title = " ")
#
# #  ------------------------------------------------------------------------