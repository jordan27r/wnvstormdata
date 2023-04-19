#' West Nile Virus Infection Data Linked with Storm Exposure
#'
#' A dataframe of West Nile Virus case counts per Parish per week in Louisiana
#'  from 2002-2021 filtered against NOAA hurricane data to determine if there
#'  was a hurricane exposure for each observation.
#'
#' @title {Primary Dataset in Package}
#' @description {WNV w/ storms}
#' @name {wnv_2002_2021_stormexposure}
#'
#' @format ## `wnv_2002_2021_stormexposure`
#' A data frame with 19,272 rows and 6 columns:
#' \describe{
#'   \item{Parish}{Parish name}
#'   \item{Fips}{5 Number Fips Cpde for Parish}
#'   \item{BeginningDate}{Date of Beginning of CDC Observance Period}
#'   \item{EndDate}{Date of End of CDC Observance Period}
#'   \item{Cases}{Number of Cases During Observance Week}
#'   \item{Exposure}{If Parish Experienced a Storm with Winds of at Least 34 Knots}
#'   \item{Storm}{If Exposed, what Storm Occured}
#'   ...
#' }
#'
#'@examples
#' data("wnv_2002_2021_stormexposure")
#' head(wnv_2002_2021_stormexposure)
#'
"wnv_2002_2021_stormexposure"
