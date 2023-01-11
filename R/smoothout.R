#-------------------------------------------------------------------------------
# Smooth out raster time-series
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Smooth raster time-series
#'
#' @description `r lifecycle::badge("stable")`
#' Smooth raster time-series and interpolate missing values using a smoother
#' function.
#'
#' @param x S4 object. The product of interest.
#' @param variable character. The variable of interest.
#' @param smoother character. A function to be applied on the original data.
#' @param pre character. A vector of dates to be used as an extra smoothing
#' window before the dates of interest.
#' @param post character. A vector of dates to be used as an extra smoothing
#' window after the dates of interest.
#' @param ... extra arguments.
#'
#' @return nothing. The smoothed data are saved directly in the cronus database.
#'
#' @importFrom terra rast app writeRaster
#' @importFrom progress progress_bar
#' @export
#'
#' @examples
#' \dontrun{
#' # Define required variables
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- date_seq("2002-01-01", "2002-12-31")
#'
#' ## MOD09GA
#'
#' # Create the object
#' x <- new("Mod09ga", region = region, date = date)
#'
#' # Smooth the raster cell time-series
#' smoothout(x, "ndvi", pre = NULL, post = NULL)
#' }
setGeneric("smoothout", signature = c("x"),
           function(x, ...) { standardGeneric("smoothout") })

#' @rdname smoothout
setMethod("smoothout",
          signature  = c(x = "Satellite"),
          definition = function(x, variable, smoother = "wt1", pre = NULL,
                                post = NULL) {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)

  # Get the toi
  toi <- get_toi(date)

  # Get the directories
  dir_var <- create_db(dir, region, product = product, variable = variable)
  path_var <- file.path(dir_var, paste0(toi$name, ".tif"))

  if (!is.null(pre)) {
    toi_pre <- get_toi(pre)
    path_var_pre <- file.path(dir_var, paste0(toi_pre$name, ".tif"))
  } else {
    path_var_pre <- NULL
  }
  if (!is.null(post)) {
    toi_post <- get_toi(post)
    path_var_post <- file.path(dir_var, paste0(toi_post$name, ".tif"))
  } else {
    path_var_post <- NULL
  }

  dir_smooth <- create_db(dir, region, product = product,
                          variable = paste0(variable, "_smooth_", smoother))
  path_smooth <- file.path(dir_smooth, paste0(toi$name, ".tif"))
  path_temp <- file.path(tempdir(), "temp.tif")

  # Smooth the rasters
  message("Smoothing Rasters")
  ras_var <- terra::rast(c(path_var_pre, path_var, path_var_post))
  temp <- terra::app(ras_var, smoother, filename = path_temp, overwrite = TRUE,
                     wopt = list(names = toi$name))

  ## Create a progress bar object
  frm <- paste0("Saving rasters [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull")
  pb <- progress::progress_bar$new(format = frm, total = length(date),
                                   clear = FALSE)

  # Save the rasters
  for (i in 1:length(date)) {
    pb$tick()
    terra::writeRaster(temp[[toi$name[i]]], filename = path_smooth[i],
                       overwrite = TRUE)
  }

})
