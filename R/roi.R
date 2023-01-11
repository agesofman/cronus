#-------------------------------------------------------------------------------
# Manage region of interest
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Get a region of interest (roi)
#'
#' @description `r lifecycle::badge("stable")`
#' This function takes the name of a region and returns a sf object.
#'
#' @param region character. The region of interest. See details
#' @param ... extra arguments
#'
#' @return An object of class sf.
#'
#' @details The function currently works only with US states and counties. The
#' object is retrieved from the U.S. Census TIGER shapefiles. Argument
#' `region` takes the form of a named character vector with elements:
#'  - `name`, the name of the region.
#'  - `type`, currently supporting "US State" and "US County".
#'  - `state`, the name of the US State. Used only for US Counties.
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom tigris states counties
#' @importFrom stringr str_to_title
#'
#' @examples
#' \dontrun{
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' get_roi(region)
#'
#' region <- Region(name = "nance", type = "us county",
#'                  div = c(country = "United States", state = "Nebraska", county = "Nance"))
#' get_roi(region)
#' }
setGeneric("get_roi", signature = c("region"),
           function(region, ...) { standardGeneric("get_roi") })

#' @rdname get_roi
setMethod("get_roi",
          signature  = c(region = "Region"),
          definition = function(region) {

  if (tolower(region@type) == "us state") {
    df_us_states <- suppressMessages(tigris::states(progress_bar = FALSE))
    roi <- dplyr::filter(df_us_states, .data$NAME == stringr::str_to_title(region@name))
  } else if (tolower(region@type) == "us county") {
    df_us_counties <- suppressMessages(tigris::counties(region@div["state"], progress_bar = FALSE))
    roi <- dplyr::filter(df_us_counties, .data$NAME == stringr::str_to_title(region@name))
  } else {
    stop("The region provided is not of valid type.")
  }
  roi

})

