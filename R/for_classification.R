# =============================================================================

#' ROC_with_cv_2
#'
#' @param Spectra_gr
#' @param Var
#' @param k
#' @param times
#' @param seeds
#' @param n_min
#'
#' @return
#' @export
#'
#' @examples
#' Spectra_gr <- Spectra
#' Var <- "CitoGr"
#' @import tidyr
#' @import dplyr
ROC_with_cv_2 <-
    function(Spectra_gr,
             Var,
             k = 5,
             times = 10,
             seeds = 2,
             n_min = k,
             range_percent = 95) {
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Remove rows with NA values and with groups which have too few samples in
        # that group
        # NA_ind <- eval_("Spectra_gr$" %++% Var) %>% is.na()
        var_values    <- getVarValues(Var, Spectra_gr)
        NA_ind        <- is.na(var_values)
        too_few_in_gr <- has_too_few_IDs(Spectra_gr, Var, n_min = n_min)
        tooFew_ind    <- var_values %in% too_few_in_gr
        Spectra_gr    <- Spectra_gr[!NA_ind & !tooFew_ind, ]

        # Drop unnecessary levels
            # eval_(paste0("Spectra_gr$", Var, " <- droplevels(Spectra_gr$", Var, ")"))
        eval_(paste0("Spectra_gr$", Var, " %<>% droplevels()"))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Make a cross-validation object
        cvo <- createFoldsBS(Spectra_gr,
                             block_by = "ID",
                             stratify_by = Var,
                             k = k,
                             times = times,
                             seeds = seeds)

        # Calculete performance
        sp_compared <- sp_class_perform_cv(sp = Spectra_gr,
                                           by = Var,
                                           measure = "SeSp",
                                           cvo = cvo)

        # =============================================================================
        # Apskaičiuojami kiekvienos grupės intensyvumų vidurkiai
        # Atsakymas ne hyperSpec, o data.frame

        n_folds <- cvo_count_folds(sp_compared$cvo)
        fold_names <- names(sp_compared$cvo)
        means_of_pairs <- data.frame()

        for (i_fold in seq.int(n_folds)) {
            inds <- cvo_get_inds(sp_compared$cvo, fold = i_fold)
            sp_means_of_groups <-
                sp_compared$data[inds, c("spc", Var)] %>%
                aggregate(. , .[[, Var, drop = TRUE]], mean, trim = .1)  %>%
                as.long.df()  %>%
                select(-one_of(Var))

            possible_gr <-
                sp_means_of_groups$.aggregate %>% levels()
            group_pairs <- combn(possible_gr, 2)
            means_of_pairs_i <- data.frame()

            for (i in 1:ncol(group_pairs)) {
                means_of_pairs_i <-  bind_rows(
                    means_of_pairs_i,
                    dplyr::filter(sp_means_of_groups, .aggregate %in% group_pairs[, i]) %>%
                        tidyr::spread(.aggregate, spc) %>%
                        setNames(c(".wavelength", "mean_1", "mean_2")) %>%
                        dplyr::mutate(Compared = paste(group_pairs[, i], collapse = " vs. "),
                               Fold = fold_names[i_fold],
                               .wavelength = round(.wavelength, 1))
                )
            }

            means_of_pairs <- dplyr::bind_rows(means_of_pairs, means_of_pairs_i)
        }
        # =============================================================================

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Cutoffs, test ir traip merformance paverčiami į data.frame
        vars_of_interest <- c("Compared", "Compared_1", "Compared_2", "Fold", "spc")
        sp_1 <-
            sp_compared$cutoffs[, vars_of_interest]  %>%
            as.long.df()  %>%
            dplyr::mutate(.wavelength = round(.wavelength, 1))  %>%
            dplyr::rename(cut_off = spc)

        sp_2 <-
            sp_compared$train_performance[, vars_of_interest]  %>%
            as.long.df()  %>%
            dplyr::mutate(.wavelength = round(.wavelength, 1))  %>%
            dplyr::rename(train_performance = spc)

        sp_3 <-
            sp_compared$test_performance[, vars_of_interest]  %>%
            as.long.df()  %>%
            dplyr::mutate(.wavelength = round(.wavelength, 1))  %>%
            dplyr::rename(test_performance = spc)

        # Sujungiami vidurkiai, cutoff, train ir test performance
        DF <- data.frame(feature = Var,
                         # sp_1  %>%
                         means_of_pairs  %>%
                             merge(sp_1) %>%
                             merge(sp_2) %>%
                             merge(sp_3))  %>%
            dplyr::arrange(feature, Fold, Compared, .wavelength)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        rm(sp_compared)
        return(DF)
    }
# =============================================================================

#' ROC_with_cv
#'
#' @param Spectra_gr
#' @param Var
#' @param k
#' @param times
#' @param seeds
#' @param n_min
#'
#' @return
#' @export
#'
#' @examples
#' Spectra_gr <- Spectra
#' Var <- "CitoGr"
#'
ROC_with_cv <-
    function(Spectra_gr,
             Var,
             k = 5,
             times = 3,
             seeds = 2,
             n_min = k,
             range_percent = 95) {
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Remove rows with NA values and with groups which have too few samples in
        # that group
        # NA_ind <- eval_("Spectra_gr$" %++% Var) %>% is.na()
        var_values    <- getVarValues(Var, Spectra_gr)
        NA_ind        <- is.na(var_values)
        too_few_in_gr <- has_too_few_IDs(Spectra_gr, Var, n_min = n_min)
        tooFew_ind    <- var_values %in% too_few_in_gr
        Spectra_gr    <- Spectra_gr[!NA_ind & !tooFew_ind, ]

        # Drop unnecessary levels
            # eval_(paste0("Spectra_gr$", Var, " <- droplevels(Spectra_gr$", Var, ")"))
        eval_(paste0("Spectra_gr$", Var, " %<>% droplevels()"))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # sp_AUC <- sp_class_perform(Spectra_gr, Var, measure = "AUC")
        # p1 <- {qplot_sp(sp_AUC$performance, by = "Compared") +
        #         set_ggLims(c(.45,1),"y") +
        #         ggtitle("AUC")
        # }  %>% ggplotly_tidy()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Make a cross-validation object
        cvo <- createFoldsBS(Spectra_gr,
                             block_by = "ID",
                             stratify_by = Var,
                             k = k,
                             times = times,
                             seeds = seeds)

        # Calculete performance
        sp_compared <- sp_class_perform_cv(sp = Spectra_gr,
                                           by = Var,
                                           measure = "SeSp",
                                           cvo = cvo)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # p2 <- sub_plot_range(sp_compared)
        # p3 <- sub_plot_means(sp_compared)
        # obj <- sp_compared
        p4 <- sub_plot_range_and_means(sp_compared, percent = range_percent)
        p5 <- cutoffs_plot(sp_compared, percent = range_percent)
        Cont <- knitrContainer()
        Cont <- Join(Cont, p4, p5)
        # Cont <- Join(Cont, p1, p2, p3)

        rez <- list(plot   = Cont,
                    object = sp_compared)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # rm(sp_AUC, sp_compared)
        rm(sp_compared)
        return(rez)
    }

#' @export
#' @rdname ROC_with_cv
plot_10 <- ROC_with_cv
# =============================================================================


# {qplot_sp(sp_compared$performance, by = "Compared") +
#     set_ggLims(c(.45,1),"y") +
#     ggtitle("Classification performance")
# } %>% plot
#
#  Spectra_gr$Compared <- Spectra_gr[[,Var, drop = TRUE]]
#     {(hyperSpec::collapse(sp_compared$cutoffs, spStat(Spectra_gr, by = Var))  %>%
#         qplot_sp(by = "Compared") +
#         ggtitle("Cut-offs"))}  %>% plot
#
# sp_AUC$cutoffs$Compared <- factor("Cut-off")

# {qplot_spRange(Spectra_gr, by = Var, percent = 50) +
#         geom_line(data=ldf(sp_AUC$cutoffs),
#                   aes_sp_string(x = ".wavelength",
#                                 y = "spc",
#                                 group = ".rownames",
#                                 size  = "Compared",
#                                 color = "Compared",
#                                 fill  = "Compared"),
#                   inherit.aes = FALSE) +
#         scale_size_manual(name = "", values = 1) +
#         scale_color_manual(values = c("black","skyblue", "orange")) +
#         scale_fill_manual( values = c("black","skyblue", "orange"))
# }  %>% ggplotly_tidy()  %>% print()

# ------------------------------------------------------------------------------
# čia taisyti - 2016-10-27


# hy_obj   <- sp_compared$test
# gg <- spStat_ci(hy_obj)
# gg %>% ggplot(aes(color = estimate)) + geom_line()

# ------------------------------------------------------------------------------
#' perf_1
#'
#' @param sp__i
#' @param sp
#' @param Var
#' @param k
#'
#' @return
#' @export
#'
#' @examples
perf_1 <- function(sp__i,sp, Var, k = 3){
    sp__i_gr <- sp__i[!is.na(sp__i[[, Var]])]
    cv_perf  <- plot_10(sp__i_gr, Var, k = k)

    sp_gr <- sp[!is.na(sp[[, Var]])]
    cv_perf_deriv <- plot_10(sp_gr, Var, k = k)

    knitrContainer()  %>%

        add_as_text("***") %>%
        add_as_heading3("Performance using spectra")  %>%
        Join(cv_perf) %>%

        add_as_text("***") %>%
        add_as_heading3("Performance using derivatives of spectra") %>%

        add_as_text("***") %>%
        Join(cv_perf_deriv)
}
