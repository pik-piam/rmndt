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

    ## mask column names in data.table expressions for code check
    myolddatacol <- myoldyearcol <- NULL

    names(data)[3] <- "myolddatacol"

    data[, (cols) := tstrsplit(myolddatacol, ".", fixed = TRUE)]
    data[, myolddatacol := NULL]

    setnames(data, c(yearcol, "value"), c("myoldyearcol", valcol))
    data[, (yearcol) := as.numeric(gsub("y", "", myoldyearcol))]
    data[, myoldyearcol := NULL]
    return(data[, c(regioncol, yearcol, cols, valcol), with=F])
}


#' Execute *vertical* calculations along a given column.
#'
#' This assumes a *long* format with a single value column,
#' dcasts the data.table to wide format, executes the calulation(s),
#' melts back to long format and returns the resulting data.table
#' with the additional column(s).
#'
#' Note that the data.table should have at least three columns, i.e.,
#' the variable, the value and one id column.
#'
#' @param dt data.table, long format
#' @param varcol name of the column with the variable
#' @param valcol name of the column with the value
#' @param expr vector of expressions to be handed to j in data.table, as strings, e.g., "a := b/c"
#' @param ... other arguments are passed on to the data.table call where `expr` is evaluated.
#'     Most likely you want to pass the `by=` parameter for group-by calls, see examples.
#'
#' @import data.table
#' @export
#' @examples
#' mt_dt <- as.data.table(mtcars, keep.rownames = TRUE)
#' ## to long
#' mt1 <- melt(mt_dt, id.vars=c("rn", "cyl"))
#'
#' varcalc_dt(mt1, "variable", "value", c("`spec. hp` := wt/hp", "wsum := sum(wt)"), by="cyl")

varcalc_dt <- function(dt, varcol, valcol, expr, ...) {
  ## cols without valcol and varcol will be idcols
  idcols <- colnames(dt)[! colnames(dt) %in% c(varcol, valcol)]
  if(length(idcols) == 0){
    stop("Table has to have a column besides variable and value columns.")
  }
  dt_wide <- data.table::dcast(dt, paste0(paste(idcols, collapse="+"), "~", varcol),
                               value.var = valcol)
  for(exp_str in expr){
    exp <- parse(text=exp_str)
    dt_wide[, eval(exp), ...]
  }
  return(melt(dt_wide, value.name=valcol,
              id.vars = idcols,
              variable.name = varcol))
}
