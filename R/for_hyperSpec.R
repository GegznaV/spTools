# Read spectra, make hyperSpec --------------------------------------------

#'  Put `Object`, `Laser_power` and `Integration_time` into one variable
#'
#' @param sp
#' @export
make_info <- function(sp){
    paste0(sp$Object,
           ";P=",    sp$Laser_power,
           ";int.t=",sp$Integration_time,"s")
}

make_info2 <- function(sp){
    paste0(sp$Object,
           ";P=",    sp$Laser_power,
           ";int.t=",sp$Integration_time,"s; ",
           sp$Point)
}

#  DEFINE merge_sp_and_diagnosis
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' merge_sp_and_diagnosis
#'
#' @param SP
#' @param add.data
#' @param by.y
#' @param by.x
#'
#' @return
#' @export
#'
#' @examples
merge_sp_and_diagnosis <- function(SP, add.data, by.y = "ID2", by.x = "Object"){
    SP <- merge(SP,
                add.data,
                by.x = by.x,
                by.y = by.y,
                all  = TRUE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    is_ThinPrep <- SP$Object %in% c("thinPrep","ThinPrep")
    levels(SP$CitoGr) <- c(levels(SP$CitoGr), "ThinPrep")
    SP$CitoGr[is_ThinPrep] <- "ThinPrep"
    SP$CitoGr <- droplevels(SP$CitoGr)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(SP)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' read_spectra
#'
#' @param path
#' @param pattern
#' @param read_fun
#' @param add.data
#' @param by.x
#' @param by.y
#' @param ...
#'
#' @export

read_spectra <- function(path,
                         pattern  = ".*", # pattern to filter files to be selected as spectra
                         read_fun = c("auto","ascii","ts","a FUNCTION")[1],
                         add.data = NULL,  # data with diagnoses.
                         by.x     = "Object",
                         by.y     = "ID2", # specification of the columns in
                         # `add.data`used for merging.
                         ... # arguments to be passed to `read_fun`
)
{

    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    # Make a list of filenames
    dir_fun  <- function(paths) dir(path = paths,
                                    pattern = pattern,
                                    full.names = TRUE)

    files <- Reduce(c, lapply(path, dir_fun))
    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    # Read spectra
    if (is.character(read_fun)) {
        read_fun <- switch(tolower(read_fun),
                           "ts"    = read.OceanView.ts0,
                           "ascii" = read.OceanView.ascii0,
                           "base32"= read.OOIBase32,
                           "auto"  = read.OceanView0,
                           stop(paste("Unsupported value of `read_fun`:", read_fun))
        )
    } else if (!is.function(read_fun)) {
        stop("`read_fun` must be either a character vector or a function")
    }

    sp <- collapse(lapply(files, read_fun, ...))
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add diagnosis
    # Add variable `ObjGr`
    if (!is.null(add.data)) {
        sp <- merge_sp_and_diagnosis(sp, add.data, by.y = by.y, by.x = by.x)

        sp$ObjGr  <-  ifelse(is.na(sp$CitoGr),
                             as.character(sp$Object),
                             as.character(sp$CitoGr))
    } else {
        sp$ObjGr <- sp$Object
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    sp$Info  <- make_info(sp)
    sp$Info2 <- make_info2(sp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(sp)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @details \code{read_spectra_ts} is a wrapper of \code{read_spectra} for
#' time series.
#' @rdname read_spectra
#' @export
read_spectra_ts <- function(path,
                            pattern  = ".*", # pattern to filter files to be selected as spectra
                            read_fun = "ts",
                            add.data = NULL,  # data with diagnoses.
                            by.x     = "Object",
                            by.y     = "ID2", # specification of the columns in
                            # `add.data`used for merging.
                            ... # arguments to be passed to `read_fun`
) {read_spectra(path, pattern, read_fun, add.data, by.y , ...)}
