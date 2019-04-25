#' Disaggregate value defined regionwise in a data.table object using a mapping.
#' If the strategy is the default (`constant`), the value for the region is used on the (ISO3) countries.
#' If the strategy is `gdp`, the GDP projections for the countries are used as weights to distribute the value for the region.
#'
#' @param data, a magpie object.
#' @param mapping, a mapping between the regions in the data and ISO3 countrycodes. *All* regions in `data` have to be part of the mapping.
#' @param regioncol, name of the column containing regions. A region is a set of countries. Default is "region".
#' @param yearcol, name of the column containing the year, default is "year".
#' @param datacols, index dimensions that are not regional or temporal dimensions.
#' @param valuecol, name of the column with the actual value to disaggregate, default is `value`.
#' @param strategy, if "gdp" is chosen, GDP projections for the countries are loaded and used as a weight to distribute the regional value.
#' @param scenario, the scenario for the GDP projections, default is `gdp_SSP2`.
#' @param usecache, if "gdp" is chosen as strategy, store the weight in a cachefile to speed up future executions.
#' @param gdpfile, if "gdp" is chosen as strategy and if caching is required, specify the filename here, default is "GDPcache.rds"
#' @keywords iso3
#' @import data.table
#' @export

toISO_dt <- function(data, mapping,
                      regioncol="region",
                      yearcol="year",
                      isocol="iso",
                      datacols="data",
                      valuecol="value",
                      strategy="constant",
                      scenario="gdp_SSP2",
                      usecache=F,
                      gdpfile="GDPcache.rds"){
    ## Note that isocol in the data has to match the column name in the mapping
    mapping <- mapping[, c(isocol, regioncol), with=F]

    ## require the mapping to be a superset of the regions in data
    diff <- setdiff(unique(data[[regioncol]]), mapping[[regioncol]])
    if(length(diff)){
        stop("Mapping is incomplete. Missing regions: ", paste(diff, collapse=", "))
    }
    ## disaggregation function
    data <- mapping[data, on=c(regioncol), allow.cartesian=T]

    if(strategy == "gdp"){
        gdp <- rmndt::getGDP_dt(scenario, yearcol, isocol, "weight",
                                usecache=usecache, gdpfile=gdpfile)
        ## GDP data ISO country resolution has to be a superset of the mapping
        diff <- setdiff(unique(mapping[[isocol]]), unique(gdp[[isocol]]))
        if(length(diff)){
            warning("Warning: GDP data is incomplete. ",
                    "ISO countries that are in the mapping, but not in the GDP data: ",
                    paste(diff, collapse=", "))
        }

        ## are data years subset of GDP data years
        stopifnot(all(data[[yearcol]] %in% gdp[[yearcol]]))

        ## leftjoin data
        data <- gdp[data, on=c(isocol, yearcol)]

        ## apply weights
        data[, (valuecol) := get(valuecol)*weight/sum(weight), by=c(yearcol, regioncol, datacols)]
        data[, c("weight", "variable") := NULL]
    }
    data[, (regioncol) := NULL]
    return(data)
}

#' Aggregate values defined countrywise in a data.table object to regions using a mapping.
#' If the strategy is the default (`sum`), the value for the region is the sum of the (ISO3) country values.
#' If the strategy is `gdp`, GDP projections for the countries are used to calculate the regional value as a weighted average of the country values.
#'
#' @param data, a magpie object.
#' @param mapping, a mapping between the regions in the data and ISO3 countrycodes. *All* regions in `data` have to be part of the mapping.
#' @param regioncol, name of the column containing regions. A region is a set of countries. Default is "region".
#' @param yearcol, name of the column containing the year, default is "year".
#' @param datacols, index dimensions that are not regional or temporal dimensions.
#' @param valuecol, name of the column with the actual value to disaggregate, default is `value`.
#' @param strategy, if "gdp" is chosen, GDP projections for the countries are loaded and used as a weight to distribute the regional value.
#' @param scenario, the scenario for the GDP projections, default is `gdp_SSP2`.
#' @param usecache, if "gdp" is chosen as strategy, store the weight in a cachefile to speed up future executions.
#' @param gdpfile, if "gdp" is chosen as strategy and if caching is required, specify the filename here, default is "GDPcache.rds"
#' @import data.table
#' @export

toRegions_dt <- function(data, mapping,
                          regioncol="region",
                          yearcol="year",
                          isocol="iso",
                          datacols="data",
                          valuecol="value",
                          strategy="sum",
                          scenario="gdp_SSP2",
                          usecache=F,
                          gdpfile="GDPcache.rds"){

    ## aggregation function, sums by default
    ## alternatively, do a weighted average by GDP
    mapping <- mapping[, c(isocol, regioncol), with=F]

    ## left join: only regions in the mapping are mapped
    data <- mapping[data, on=c(isocol)]

    ## require the mapping to be a superset of the countries in data
    diff <- setdiff(unique(data[[isocol]]), mapping[[isocol]])
    if(length(diff)){
        warning("Mapping is incomplete. Data for the following ISO countries is omitted: ", paste(diff, collapse=", "))
        data <- data[!is.na(get(regioncol))]
    }

    if(strategy == "gdp"){
        gdp <- rmndt::getGDP_dt(scenario, yearcol, isocol, "weight",
                                usecache=usecache, gdpfile=gdpfile)

        diff <- setdiff(unique(mapping[[isocol]]), unique(gdp[[isocol]]))
        if(length(diff)){
            warning("Warning: GDP data is incomplete. ",
                    "ISO countries that are in the mapping, but not in the GDP data: ",
                    paste(diff, collapse=", "))
        }
        ## are data years subset of GDP data years
        stopifnot(all(data[[yearcol]] %in% gdp[[yearcol]]))

        ## leftjoin data
        data <- gdp[data, on=c(isocol, yearcol)]

        ## apply weights
        data[, (valuecol) := get(valuecol)*weight/sum(weight), by=c(yearcol, regioncol, datacols)]

        data[, c("weight", "variable") := NULL]

    }
    ## sum
    data[, (valuecol) := sum(get(valuecol), na.rm=T), by=c(yearcol, regioncol, datacols)]

    # drop isocol
    data[, (isocol) := NULL]
    # drop duplicates
    data <- unique(data[, c(regioncol, yearcol, datacols, valuecol), with=FALSE])
    return(data)
}

