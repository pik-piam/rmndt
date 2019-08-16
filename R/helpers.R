#' Load a magpie object as data.table object with given colnames.
#' Replaces years by numeric values, removing the leading y.
#'
#' @param data a magpie object.
#' @param datacols the names of the data dimension(s) of the magpie object.
#'            If no value is given, the name provided in the magpie object is used.
#' @param regioncol name of the column containing REMIND regions, default is "region".
#' @param yearcol name of the column containing the year, default is "year".
#' @param valcol column to host actual value, default is "value"
#'
#' @import data.table
#' @importFrom magclass getSets getSets<-
#' @export
#' @examples
#' \dontrun{
#' require(magpie)
#' dt <- magpie2dt(population_magpie)
#' }

magpie2dt <- function(data, regioncol=NULL, yearcol=NULL, datacols=NULL, valcol="value") {
    if(is.null(regioncol)){
        regioncol <- getSets(data)[1]
    }else{
        getSets(data)[1] <- regioncol
    }
    if(is.null(yearcol)){
        yearcol <- getSets(data)[2]
    }else{
        getSets(data)[2] <- yearcol
    }

    data <- as.data.table(data)
    cols <- unlist(strsplit(names(data)[[3]], ".", fixed = TRUE))
    if(!is.null(datacols)){
        stopifnot(length(datacols) == length(cols))
        cols <- datacols
    }

    names(data)[3] <- "myolddatacol"

    data[, (cols) := tstrsplit(myolddatacol, ".", fixed = TRUE)]
    data[, myolddatacol := NULL]

    setnames(data, c(yearcol, "value"), c("myoldyearcol", valcol))
    data[, (yearcol) := as.numeric(gsub("y", "", myoldyearcol))]
    data[, myoldyearcol := NULL]
    return(data[, c(regioncol, yearcol, cols, valcol), with=F])
}
