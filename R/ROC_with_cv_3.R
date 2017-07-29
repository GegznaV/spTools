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
#'
#' @import tidyr
#' @import dplyr
#' @import glue
#'
# Rename to `sp_uniroc`
ROC_with_cv_3 <-
    function(Spectra_gr,
             Var,
             k = 5,
             times = 10,
             seeds = 2,
             # reikia įdėti seed generatoriaus pavadinimą
             n_min = k,
             range_percent = 95



             ) {
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Remove rows with NA values and with groups which have too few samples in
        # that group

        var_values    <- getVarValues(Var, Spectra_gr)
        NA_ind        <- is.na(var_values)
        too_few_in_gr <- has_too_few_IDs(Spectra_gr, Var, n_min = n_min)
        tooFew_ind    <- var_values %in% too_few_in_gr
        Spectra_gr    <- Spectra_gr[!NA_ind & !tooFew_ind, ]

        # Drop unnecessary levels
        eval_(glue::glue("Spectra_gr${Var} %<>% droplevels()"))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Make a cross-validation object
        cvo <- cvo_create_folds(Spectra_gr,
                                block_by = "ID",
                                stratify_by = Var,
                                k = k,
                                times = times,
                                seeds = seeds)

# base::save.image("~/__tmp-data__/tmp.RData")
        # Calculete performance
        sp_compared <- sp_class_perform_cv(sp = Spectra_gr,
                                           by = Var,
                                           measure = "BAC",
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
        # =====================================================================

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        rm(sp_compared)
        return(DF)
    }
# =============================================================================