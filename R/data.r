#' West Nile Virus Infection Data Linked with Storm Exposure
#'
#' A dataframe of West Nile Virus case counts per Parish per week in Louisiana
#'  from 2002-2021 filtered against NOAA hurricane data to determine if there
#'  was a hurricane exposure for each observation.
#'
#'
#' @format ## `wnv_2002_2021_stormexposure`
#' A data frame with 19,272 rows and 6 columns:
#' \describe{
#'   \item{Parish}{Character string with Parish name}
#'   \item{Fips}{Character string with 5 Number FIPs Cpde for Parish}
#'   \item{BeginningDate}{Date Object with Beginning of CDC Epidemiological Week}
#'   \item{EndDate}{Date Object with End of CDC Epidemiological Week}
#'   \item{Cases}{Double object with Number of Cases During Epidemiological Week}
#'   \item{Exposure}{Double Object of Parish Exposure to a Storm with Winds of at Least 34 Knots, Denoted 1 for Exposure}
#'   \item{Storm}{Characyer Obeject of what Stomr Occured if Exposed}
#' }
#'
#'@examples
#' data("wnv_2002_2021_stormexposure")
#' head(wnv_2002_2021_stormexposure)
#'
"wnv_2002_2021_stormexposure"
