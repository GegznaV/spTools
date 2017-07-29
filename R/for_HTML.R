#' rm_HTML_tags
#'
#' @param htmlString text
#'
#' @return
#' @export
#'
rm_HTML_tags <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
}

require(htmltools)

insert_fig_href <- function(file, caption, width = NULL,
                            fig.align, text.align, text.style,
                            class = "figure",
                            overwrite = FALSE,
                            quality = 85)
{
    caption <- if (nchar(file) == 0) caption else iFig_(caption)

    div(class = class,
        style = sprintf('text-align:%s; font-style:%s;',fig.align, text.style),
        a(target = "_self",
          href   = file,
          img(src = create_thumbnail(file, w = width, h = NULL, ratio = NULL,
                                     overwrite, quality),
              alt = (caption %>%
                         rm_HTML_tags %>%
                         make.names %>%
                         strtrim(width = 50)
              )
          )
        ),
        p(class = "caption", align = text.align, HTML(caption))
    )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Fig
#'
#' @param file
#' @param caption
#' @param fig.align
#' @param width
#' @param text.align
#' @param text.style
#' @param class
#' @param overwrite
#' @param quality
#'
#' @export
Fig <- function(file  = "",
                caption = "",
                fig.align  = c("center","left","right","justify")[1],
                width   = 500,
                text.align = c("center","justify","left","right")[1],
                text.style = c("italic","normal","oblique")[1],
                class  = "figure",
                overwrite = FALSE,
                quality = 85)
{    insert_fig_href(file,caption,width,
                     fig.align, text.align, text.style,
                     class, overwrite, quality)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Fig
#' @export
Fig_1col <- function(file   = "",
                     caption = "",
                     width = 300,
                     fig.align  = c("center","left","right","justify")[1],
                     text.align = c("justify","center","left","right")[1],
                     text.style = c("italic","normal","oblique")[1],
                     class = "col-sm-12",
                     overwrite = FALSE,
                     quality = 85)
{insert_fig_href(file, caption, width,
                 fig.align, text.align,text.style,
                 class,overwrite, quality)}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Fig
#' @export
Fig_2cols <- function(file   = "",
                      caption = "",
                      width = 300,
                      fig.align  = c("center","left","right","justify")[1],
                      text.align = c("justify","center","left","right")[1],
                      text.style = c("italic","normal","oblique")[1],
                      class = "col-sm-6",
                      overwrite = FALSE,
                      quality = 85)
{insert_fig_href(file, caption, width,
                 fig.align, text.align,text.style,
                 class,overwrite, quality)}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Fig
#' @export
Fig_3cols <- function(file   = "",
                      caption = "",
                      width = 300,
                      fig.align  = c("center","left","right","justify")[1],
                      text.align = c("justify","center","left","right")[1],
                      text.style = c("italic","normal","oblique")[1],
                      class = "col-sm-4",
                      overwrite = FALSE,
                      quality = 85)
{insert_fig_href(file, caption, width,
                 fig.align, text.align,text.style,
                 class, overwrite, quality)}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Deprecated:
#' @rdname Fig
#' @export
insert_fig_2cols <- function(file   = "",
                             caption = "",
                             width = 300,
                             fig.align  = c("center","left","right","justify")[1],
                             text.align = c("justify","center","left","right")[1],
                             text.style = c("italic","normal","oblique")[1],
                             class = "col-sm-6",
                             overwrite = FALSE,
                             quality = 85)
{insert_fig_href(file, caption, width,
                 fig.align, text.align, text.style,
                 class, overwrite, quality)}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Deprecated:
#' @rdname Fig
#' @export
insert_fig_3cols <- function(file   = "",
                             caption = "",
                             width = 300,
                             fig.align  = c("center","left","right","justify")[1],
                             text.align = c("justify","center","left","right")[1],
                             text.style = c("italic","normal","oblique")[1],
                             class = "col-sm-4",
                             overwrite = FALSE,
                             quality = 85)
{insert_fig_href(file, caption, width,
                 fig.align, text.align,text.style,
                 class, overwrite, quality)}


