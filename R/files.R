#-------------------------------------------------------------------------------
# Get files
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Get files
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Get the file paths needed to derive a variable.
#'
#' @param x S4 object. A product of interest.
#' @param y S4 object. A product of interest.
#' @param variable character. The variable of interest.
#' @param varxy character. Variables used to derive \code{variable}.
#' @param ... extra arguments.
#'
#' @return A vector or list with the paths required by \code{derive}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define required variables
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- date_seq("2002-01-01", "2002-12-31")
#'
#' # Create the object
#' x <- new("Mod09ga", region = region, date = date)
#'
#' # Get the files
#' get_files(x, "ndvi")
#' }
setGeneric("get_files", signature = c("x", "y"),
           function(x, y, ...) { standardGeneric("get_files") })

#' @rdname get_files
setMethod("get_files",
          signature  = c(x = "Mod09ga"),
          definition = function(x, variable) {

  if (variable == "ndvi") {
    files <- c("MODIS_Grid_500m_2D_sur_refl_b01_1.tif",
               "MODIS_Grid_500m_2D_sur_refl_b02_1.tif")
  } else if (variable == "cloudmask") {
    files <- "MODIS_Grid_1km_2D_state_1km_1.tif"
  }

  files

})

#' @rdname get_files
setMethod("get_files",
          signature  = c(x = "Daymet", y = "Cropmaps"),
          definition = function(x, y, variable, varxy) {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  productx <- get_product(x)
  producty <- get_product(y)

  # Get the toi
  toi <- get_toi(date)
  files <- list()

  if (variable == "gdd") {
    dir_tmin <- create_db(dir, region, product = productx, variable = varxy[1])
    dir_tmax <- create_db(dir, region, product = productx, variable = varxy[2])
    dir_cdl <- create_db(dir, region, product = producty, variable = varxy[3])

    for (i in 1:length(date)) {
      files[[i]] <- c(file.path(dir_tmin, paste0(toi$name[i], ".tif")),
                      file.path(dir_tmax, paste0(toi$name[i], ".tif")),
                      file.path(dir_cdl, paste0(toi$year[i], ".tif")))
    }
  }

  files

})
