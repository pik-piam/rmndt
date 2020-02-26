#' The mapping between REMIND-12 regions and ISO countries
#'
#' @format A data frame with 249 rows and 3 columns:
#' \describe{
#'   \item{name}{Name of the country}
#'   \item{iso}{ISO3 country code}
#'   \item{region}{REMIND region name}
#' }
"REMIND_RegionMap"

#' REMIND GDP Trajectories for ISO countries
#'
#' @format A data frame with 9462 rows and 4 columns:
#' \describe{
#'   \item{iso}{ISO3 country code}
#'   \item{variable}{SSP GDP variant, this is gdp_SSP2 in this case}
#'   \item{weight}{GDP in US$2010}
#'   \item{year}{Year}
#' }
"REMIND_GDP"

#' A random REMIND FE trajectory.
#'
#' @format A data frame with 2011 rows and 6 columns:
#' \describe{
#'   \item{year}{REMIND time step}
#'   \item{region}{REMIND-12 region}
#'   \item{se}{Secondary energy identifier}
#'   \item{fe}{Final energy identifier}
#'   \item{te}{Conversion technology identifier}
#'   \item{value}{The data column, EJ/yr}
#' }
"REMIND_FinalEnergy"
