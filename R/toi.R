#-------------------------------------------------------------------------------
# Manage time of interest
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Manage time of interest (`toi`)
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Create a list that contains all the necessary formats of the time of interest.
#'
#' @param date character. A vector of dates.
#' @param from character. First date.
#' @param to character. Last date.
#' @param by character. Increment of sequence, e.g. day
#' @param doy numeric. A vector of days-of-year.
#' @param pdoy character. A vector of three-digit day-of-year (padded with zeros).
#' @param year numeric. A vector of years.
#'
#' @return
#'  - `get_toi()` returns a list with elements `date`, `year`, `uyear`, `month`, `doy`, `pdoy`, and `name`.
#'  - `date_seq()`, `doy_to_date()`, `pdoy_to_date()` return a sequence of Dates.
#'
#' @export
#' @importFrom lubridate year month yday
#' @importFrom stringr str_pad
#'
#' @examples
#' \dontrun{
#' dates <- date_seq("2002-01-01", "2002-01-04")
#' toi <- get_toi(dates)
#' doy_to_date(c(1, 365), c(2002, 2005))
#' pdoy_to_date(c("2002001", "2005365"))
#' }
get_toi <- function(date) {
  toi <- list(date = date)
  toi$year <- lubridate::year(date)
  toi$uyear <- unique(toi$year)
  toi$month <- lubridate::month(date)
  toi$doy <- lubridate::yday(date)
  toi$pdoy <- stringr::str_pad(toi$doy, 3, pad = "0")
  toi$name <- paste0(toi$year, toi$pdoy)
  toi
}

#' @export
#' @rdname get_toi
date_seq <- function(from, to, by = "day") {
  seq(from = as.Date(from), to = as.Date(to), by = by)
}

#' @rdname get_toi
doy_to_date <- function(doy, year) {
  as.Date(as.numeric(doy), origin = paste0(year, "-01-01")) - 1
}

#' @rdname get_toi
pdoy_to_date <- function(pdoy) {
  year <- substr(pdoy, 1, 4)
  doy <- substr(pdoy, 5, 7)
  doy_to_date(doy, year)
}
