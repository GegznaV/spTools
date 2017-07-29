# plot Crosstabs --------------------------------------------------------------

#' plot_crosstabs_sp
#'
#' @param sp
#' @param vars
#' @param vars2
#'
#' @return
#' @export
#'
#' @examples
#'
plot_crosstabs_sp <- function(sp, vars, vars2 = NULL) {
    # Combinations by 2 - variable 1 vs. variable 2:
    By_2 <- if (is.null(vars2))
    {
        combn(vars, 2)  %>% t
    } else {
        expand.grid(vars, vars2) %>% as.matrix
    }

    for (i in 1:nrow(By_2)) {
        x_var <- By_2[i, 1]
        y_var <- By_2[i, 2]
        cat(paste(labels(sp, x_var),
                  " vs. ",
                  labels(sp, y_var),  "\n"))
        gg_crosstab2(sp, x_var, y_var)
    }
}
# =============================================================================
# Print summary of indicated variable ------------------------------------------
#
# ID - variable with IDs of medical samples.
# gr - name of factor variable.
#
#' Print summary of indicated variable
#'
#' @param sp
#' @param ID variable with IDs of medical samples.
#' @param gr name of factor variable.
#'
#' @return
#' @export
#'
#' @examples
print_gr_summary <- function(sp, ID = "ID", gr = "gr")
{
    pander::pander(nID_nSp(data = sp, ID = ID, gr = gr),
                   caption = "Distribution of Specimens and Spectra by"  %.+.%
                       labels(sp, gr))
}
# ------------------------------------------------------------------------------

#' @name ggdensity
#' @title ggdensity
#'
#' @param sp
#' @param x
#' @param gr
#'
#' @return
#' @export
#'
#' @examples
ggdensity <- function(sp, x = "age", gr = NULL){
    ggplot(ldf(sp)) +
        geom_density(aes_string(x = x, fill = gr), alpha = .3)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name ggdensity
#' @export
ggdensity_3subplots <- function(sp,
                                x = "age",
                                gr = c("Hybrid_normal",
                                       "Hybrid_cervicitis",
                                       "Hybrid_cin3_cis")
) {
    knitrContainer() %>%
        add_as_is(ggdensity(sp,x,gr[1]) %>% ggplotly_tidy())  %>%
        add_as_is(ggdensity(sp,x,gr[2]) %>% ggplotly_tidy())  %>%
        add_as_is(ggdensity(sp,x,gr[3]) %>% ggplotly_tidy())
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name ggdensity
#' @export
ggdensity_age <- function(sp,
                          x  = "age",
                          gr = "Hybrid_normal"){
    knitrContainer() %>%
        add_as_heading3("Distribution of" %.+.% x)  %>%
        add_as_is(ggdensity(sp,x,gr))
}


