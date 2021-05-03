#' Read a REMIND output (MIF) file.
#'
#' REMIND style output files are semi-colon separated CSVs with a trailing semi-colon
#' at the end of each row. The following structure is assumed:
#' Columns "Model", "Scenario", "Region", "Variable", "Unit"
#' and an arbitrary number of year colums (convertable to numeric).
#'
#' @param mif A REMIND output file (.MIF)
#'
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' dt <- readMIF("REMIND_generic_default.mif")
#' }

readMIF <- function(mif) {
  dt <- fread(mif, header=T)

  cols <- colnames(dt)[6:length(colnames(dt))]
  for(col in cols){
    ncol <- suppressWarnings(as.numeric(col))
    if(is.na(ncol)){
      dt[, (col) := NULL]
    }
  }
  return(dt)
}

#' Write a REMIND output (MIF) file.
#'
#' Note that these files are semi-colon separated CSVs with a trailing semi-colon
#' at the end of each entry. Required columns are "Model", "Scenario",
#' "Region", "Variable", "Unit" and an arbitrary number of year colums
#' (should be convertable to numeric).
#'
#' @param dt a data.table in the correct format.
#' @param destination path to the resulting MIF file
#' @param append append to an existing MIF file?
#' @param ... other parameters are passed on to data.table::fwrite
#'
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' writeMIF(dt, "REMIND_generic_default.mif")
#' }

writeMIF <- function(dt, destination, append=FALSE, ...) {
  ## Check for columns
  if(!all(colnames(dt)[1:5] == c("Model", "Scenario", "Region", "Variable", "Unit"))){
    stop(paste("Supplied data.table does not support the correct column names.",
               "There should be `Model`, `Scenario`, `Region`, `Variable`, `Unit`"))
  }
  ## try to convert remaining cols to numerics
  chk <- sapply(colnames(dt)[6:length(colnames(dt))], as.numeric)

  EOL <- if (.Platform$OS.type=="windows") ";\r\n" else ";\n"
  fwrite(dt, destination, append=append, sep=";", eol=EOL, ...)
}


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
#' @export
#' @examples
#' \dontrun{
#' require(magpie)
#' dt <- magpie2dt(population_magpie)
#' }

magpie2dt <- function(data, regioncol=NULL, yearcol=NULL, datacols=NULL, valcol="value") {
    if(is.null(regioncol)){
        regioncol <- magclass::getSets(data)[1]
    }else{
        magclass::getSets(data)[1] <- regioncol
    }
    if(is.null(yearcol)){
        yearcol <- magclass::getSets(data)[2]
    }else{
        magclass::getSets(data)[2] <- yearcol
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
