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
#'   \item{parish}{Character string with Parish name}
#'   \item{fips}{Character string with 5 Number FIPs Cpde for Parish}
#'   \item{startDate}{Date Object with Beginning of CDC Epidemiological Week}
#'   \item{endDate}{Date Object with End of CDC Epidemiological Week}
#'   \item{cases}{Double object with Number of Cases During Epidemiological Week}
#'   \item{exposure}{Double Object of Parish Exposure to a Storm with Winds of at Least 34 Knots, Denoted 1 for Exposure}
#'   \item{storm}{Characyer Obeject of what Storm Occured if Exposed}
#'   \item{wind}{Numeric Object with Max Sustained Wind in Knots if Storm Exposure Occured}
#' }
#'
#'@examples
#' data("wnv_2002_2021_stormexposure")
#' head(wnv_2002_2021_stormexposure)
#'
"wnv_2002_2021_stormexposure"
