#' Approximate missing values in a data.table.
#'
#' Similar to, but not quite like, `stats::approx`.
#' Does only support constant extrapolation and linear interpolation.
#' The resulting `data.table` only contains the range provided by `xdata` along `xcol`.
#' Without extrapolation, `xcol` in the resulting `data.table` may not
#' cover the range given by `xdata`.
#'
#' @param dt a data.table.
#' @param xdata the range to interpolate to. This is the range the result will have along the dimension `xcol`.
#' @param xcol name of the column for interpolation.
#' @param ycol name of the column that contains the value to be interpolated.
#' @param idxcols columns that identify a row (besides xcol), i.e., the remaining index dimensions.
#' @param keepna keep NA values for rows that can not be interpolated (since they are outside of [min(xcol), max(xcol)]), default is FALSE.
#' @param extrapolate use the closest values to fill `ycol` outside of the interpolation domain, default is FALSE. This will also work if there is only one value along `ycol`, i.e., no interpolation is taking place.
#' @return a data.table with the range given by `xdata` along `xcol`. Columns not given in `idxcols` will be kept but NAs will appear on extrapolated and interpolated rows.
#'
#' @import data.table
#' @importFrom stats approx
#' @export
#' @examples
#' dt <- as.data.table(ChickWeight)
#' ## delete all values but 1
#' dt[Chick == 1 & Time > 0, weight := NA]
#' ## delete all values but 2
#' dt[Chick == 2 & Time > 2, weight := NA]
#'
#' ## extrapolation from 1 value
#' approx_dt(dt, 0:21, "Time", "weight", idxcols=c("Chick", "Diet"), extrapolate = TRUE)[Chick == 1]
#' ## extrapolation and interpolation
#' approx_dt(dt, 0:21, "Time", "weight", idxcols=c("Chick", "Diet"), extrapolate = TRUE)[Chick == 2]
#' ## column not in idxcols
#' approx_dt(dt, 0:21, "Time", "weight", idxcols="Chick", extrapolate = TRUE)[Chick == 2]
#'
#' dt <- as.data.table(ChickWeight)
#' ## interpolation only
#' approx_dt(dt, 0:21, "Time", "weight", idxcols=c("Chick", "Diet"))[Chick == 2]

approx_dt <- function(dt, xdata, xcol, ycol,
                      idxcols=NULL,
                      keepna=FALSE,
                      extrapolate=FALSE){

    ## assert that there is some overlap between given xdata and the values in xcol
    if(!any(between(dt[[xcol]], min(xdata), max(xdata)))){
        stop("Given xdata and range in the xcol column of the table are not overlapping.")
    }

    ## create a datatable based on the index columns and the new xrange
    vectors <- lapply(idxcols, function(item){
        dt[[item]]
    })
    names(vectors) <- idxcols

    ## missing_xvals = setdiff(xdata, dt[[xcol]])
    vectors[[xcol]] <- xdata

    ## for the missing years we expand the full idx range
    result <- merge(dt, do.call(CJ, c(vectors, unique=T)), by=c(idxcols, xcol), all=T)

    ## we delete combinations that are all NAs
    result = result[result[, .I[!all(is.na(get(ycol)))], by=idxcols]$V1]


    if(extrapolate){
        result[, (ycol) := if(sum(!is.na(.SD[[ycol]])) > 1){
                               ## if there are at least two non-NA values, we interpolate
                               approx(.SD[[xcol]], .SD[[ycol]], xout = .SD[[xcol]], rule = 2)$y
                           }else{
                               ## if there is only one value, we use it on the whole column
                               sum(.SD[[ycol]], na.rm = T)
                           },
               by=idxcols]
    }else{
        if(max(dt[[xcol]]) < max(xdata) || min(dt[[xcol]]) > min(xdata)){
            stop("Error: interpolation range out of bounds.")
        }
        result[, (ycol) := approx(.SD[[xcol]], .SD[[ycol]], xout = .SD[[xcol]], rule = 1)$y,
               by=idxcols]
        if(!keepna){
            ## in case no extrapolation is taking place, we might want to
            ## remove NAs from the result
            result <- result[!is.na(get(ycol))]
        }
    }

    ## we will filter the result using join
    jdt <- data.table(xdata)
    setnames(jdt, xcol)
    return(result[jdt, on=xcol])
}
