#' Load GDP data using `moinput` on ISO country resolution for a scenario as data.table object with given colnames.
#' By default, the 
#'
#' @param scenario, GDP scenario, default is gdp_SSP2.
#' @param yearcol, name of the year column, default "year".
#' @param isocol, name of the column containing ISO3 codes, default is "iso".
#' @param valuecol, name of the column containing the GDP values, default is "weight".
#' @param usecache, store the result in a RDS file in the working directory, default is FALSE.
#' @param gdpfile, if caching is required, specify the filename here, default is "GDPcache.rds"
#' @keywords gdp
#' @import data.table
#' @export
#' @examples
#' gdp <- getGDP_dt()

getGDP_dt <- function(scenario="gdp_SSP2",
                      yearcol="year",
                      isocol="iso",
                      valuecol="weight",
                      usecache=F,
                      gdpfile="GDPcache.rds"){

    if(usecache && file.exists(gdpfile)){
        cat("getGDP_dt: Using cached GDP data in", gdpfile, "\n")
        return(readRDS(gdpfile))
    }

    GDPppp_country <- madrat::calcOutput("GDPppp", aggregate = F)[,, scenario]

    gdp <- as.data.table(GDPppp_country)[variable == scenario]
    gdp[, (yearcol) := as.numeric(gsub("y", "", Year))][, Year := NULL]
    setnames(gdp, c("ISO3", "value"), c(isocol, valuecol))

    if(usecache){
        saveRDS(gdp, gdpfile)
    }

    return(gdp)
}

#' Load a magpie object as data.table object with given colnames.
#' Replaces years by numeric values, removing the leading y.
#'
#' @param data, a magpie object.
#' @param datacols, the names of the data dimension(s) of the magpie object.
#'            If no value is given, the name provided in the magpie object is used.
#' @param regioncol, name of the column containing REMIND regions, default is "region".
#' @param yearcol, name of the column containing the year, default is "year".
#' @import data.table
#' @export
#' @examples
#' dt <- magpie2dt(myMagpieObject)

magpie2dt <- function(data, regioncol=NULL, yearcol=NULL, datacols=NULL) {
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

    setnames(data, yearcol, "myoldyearcol")
    data[, (yearcol) := as.numeric(gsub("y", "", myoldyearcol))]
    data[, myoldyearcol := NULL]
    return(data[, c(regioncol, yearcol, cols, "value"), with=F])
}

