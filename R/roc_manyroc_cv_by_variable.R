#' # =============================================================================
#' ## library(parallelMap)
#' #
#' # parallelStartSocket(4)
#' #
#' # parallelLibrary("purrr", "manyROC","spTools", "spHelper", show.info = FALSE)
#' # parallelExport("sp_roc_with_cv_3", "gr_df", "spectra_mat",
#' #                "k_folds", "times", show.info = FALSE)
#' #
#' # parallelStop()
#'
#' #' sp_manyroc_cv_by_variable
#' #'
#' #' @param spectra_mat
#' #' @param gr_df
#' #' @param k_folds
#' #' @param times
#' #' @param seeds
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' sp_manyroc_cv_by_variable(spectra_mat2, "gr")
#' roc_manyroc_cv_by_variable <- function(spectra_mat,
#'                                         gr_df,
#'                                         k_folds = 3,
#'                                         times = 10,
#'                                         seeds = 2222222) {
#' # -----------------------------------------------------------------------------
#'     checkmate::assert_matrix(spectra_mat)
#'     checkmate::assert_data_frame(gr_df)
#' # -----------------------------------------------------------------------------
#'     # if (!checkmate::test_named(gr_df))
#'     #     names(gr_df) <- gr_df
#' # -----------------------------------------------------------------------------
#'     rez_tmp <- parallelMap::parallelLapply(gr_df,
#'                                        purrr::safely(sp_roc_with_cv_4),
#'                                        spectra_mat_gr = spectra_mat,
#'                                        k = k_folds,
#'                                        times = times,
#'                                        seeds = seeds)
#' # -----------------------------------------------------------------------------
#'     not_error <- purrr::map_lgl(rez_tmp, ~is.null(.x$error))
#'     t_tez <- purrr::transpose(rez_tmp)
#'
#'     rez_not_err <- t_tez$result[not_error]
#'     names(rez_not_err) <- purrr::map_chr(rez_not_err, ~.x$variable)
#'     rez_final <- purrr::transpose(rez_not_err)
#'
#'     rez_final$results <- dplyr::bind_rows(rez_final$results, .id = "grouping")
#'
#'     rez_final$ind_included_rows %<>% as.data.frame() %>% add_class_label("roc_df")
#'     rez_final$n_included %<>% dplyr::bind_cols() %>% as.data.frame()
#'
#'     rez_final$variables_included <- rez_final$variable %>% purrr::reduce(c)
#'     rez_final$variables_errored <-
#'         gr_df[!not_error] %>% remove_names()
#'
#'     rez_final$error_messages <- t_tez$error
#' # -----------------------------------------------------------------------------
#'     rez_final
#'
#' }
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' remove_names <- function(x) {
#'     names(x) <- NULL
#'     x
#' }
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' # =============================================================================
#' # spectra_mat_gr <- spectra_mat
#' # Var_df <- "CitoGr"
#' sp_roc_with_cv_4 <-
#'     function(Var_df,
#'              spectra_mat_gr,
#'              k = 5,
#'              times = 10,
#'              seeds = 2222222,
#'              # reikia įdėti seed generatoriaus pavadinimą
#'              n_min = k
#'              ) {
#'
#'         # # ID <- Spectra$ID
#'         #
#'         #
#'         # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'         # # Remove rows with NA values and with groups which have too few samples in
#'         # # that group
#'         #
#'         # var_values    <- get_var_values(Var_df, spectra_mat_gr)
#'         # var_values    <- Var_df
#'         #
#'         #
#'         # too_few_in_gr <- has_too_few_IDs(spectra_mat_gr, Var_df, n_min = n_min)
#'         # ind_too_few   <- var_values %in% too_few_in_gr
#'         # ind_NA        <- is.na(var_values)
#'         # ind_included_rows  <- !ind_NA & !ind_too_few
#'         # spectra_mat_gr    <- spectra_mat_gr[ind_included_rows, ]
#'         #
#'         # # Drop unnecessary levels
#'         # eval_glue("spectra_mat_gr$`{Var_df}` %<>% droplevels()")
#'         # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'         # # Make a cross-validation object
#'         # cvo <- cvo_create_folds(spectra_mat_gr,
#'         #                         block_by = "ID",
#'         #                         stratify_by = Var_df,
#'         #                         k = k,
#'         #                         times = times,
#'         #                         seeds = seeds,
#'         #                         kind = "L'Ecuyer-CMRG")
#'         #
#'         # x  <- spectra_mat_gr[[]]
#'         # gr <- spectra_mat_gr[[,Var_df, drop = TRUE]]
#'         #
#'         # roc_rez <- roc_manyroc_cv(x = x, gr = gr, optimize_by = "bac", cvo = cvo)
#'         #
#'         # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'         # list(variable = Var_df,
#'         #      n_included = sum(ind_included_rows),
#'         #      ind_included_rows = add_class_label(ind_included, "as_str"),
#'         #      # x  = add_class_label(x, "as_str"),
#'         #      # gr = add_class_label(gr, "as_str"),
#'         #      cvo = cvo,
#'         #
#'         #      results = add_class_label(roc_rez, "roc_df"))
#'     }
#'
#' # =============================================================================
#' # library(parallelMap)
#' #
#' # parallelStartSocket(4)
#' #
#' # parallelLibrary("purrr", "manyROC","spTools", "spHelper", show.info = FALSE)
#' # parallelExport("sp_roc_with_cv_3", "gr_df", "spectra_mat",
#' #                "k_folds", "times", show.info = FALSE)
#' #
#' # parallelStop()
#'
#' # =============================================================================
#'
#' # mutate(.wavelength = as.numeric(feature),
#' #        set = forcats::fct_rev(set))  %>%
#'
#' # =============================================================================
#' #
#' #
#'
#' # tmp <- spectra_mat_gr[,c("spc",Var_df),453]
#' # roc_analysis(tmp[[]], tmp[[,Var_df, drop = TRUE]])
#' #
#' # # ======================================================================
#' # q025 <- purrr::partial(quantile, probs = 0.025, na.rm = TRUE)
#' # q975 <- purrr::partial(quantile, probs = 0.975, na.rm = TRUE)
#' #
#' # _q025 <- purrr::partial(quantile, probs = 0.025, na.rm = TRUE)
#' # _q975 <- purrr::partial(quantile, probs = 0.975, na.rm = TRUE)
#' # _mean <- purrr::partial(mean,  na.rm = TRUE)
#' #
#' #
#' # roc_rez  %>%
#' #     group_by(compared_groups, set, .wavelength)  %>%
#' #     summarize_at(
#' #         vars(
#' #             # sens,
#' #             # spec,
#' #             ppv,
#' #             npv,
#' #             # bac,
#' #             # youden,
#' #             # kappa,
#' #             auc),
#' #         funs("mean"))  %>%
#' #
#' #     gather(key = "Measure",
#' #            value = "value",
#' #            # sens,
#' #            # spec,
#' #            ppv,
#' #            npv,
#' #            # bac,
#' #            # youden,
#' #            # kappa,
#' #            auc) %>%
#' #
#' #     ggplot(aes(.wavelength, value, color = Measure)) +
#' #     geom_hline(yintercept = 0.75, lty = 2, color = "grey20") +
#' #     geom_hline(yintercept = c(0, 0.5), lty = 2, color = "red4") +
#' #     geom_point() +
#' #     geom_line(size = 1) +
#' #     facet_grid(compared_groups ~ set) +
#' #     ylim(c(-0.15, 1))
#' #
#' # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # ggplotly( roc_rez  %>%
#' #               group_by(compared_groups, set, .wavelength)  %>%
#' #               summarize_at(
#' #                   vars(median_neg, cutoff, median_pos),
#' #                   funs(q025, mean, q975))  %>%
#' #               as.data.frame()  %>%
#' #
#' #               gather(key = "Measures",
#' #                      value = "value",
#' #                      -compared_groups, -set, -.wavelength) %>%
#' #               separate(col = "Measures",
#' #                        into = c("Measure", "stat"),
#' #                        sep = "_(?=[^np])") %>%
#' #               spread(key = "stat", value = "value")  %>%
#' #
#' #               add_class_label("roc_df")  %>%
#' #
#' #               ggplot(aes(.wavelength, color = Measure, fill = Measure)) +
#' #               # geom_hline(yintercept = 0.75, lty = 2, color = "grey20") +
#' #               # geom_hline(yintercept = c(0, 0.5), lty = 2, color = "red4") +
#' #               geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.3) +
#' #               geom_line(aes(y = mean), size = 1) +
#' #               facet_grid(compared_groups ~ set))
#'
#'
#' # +
#' #     ylim(c(-0.15, 1))
#'
#' # %>%
#' # summarize_at(vars(sens, spec,  ppv,  npv,  bac, youden, kappa,  auc),
#' #              funs(q025, mean, q975))
#'
