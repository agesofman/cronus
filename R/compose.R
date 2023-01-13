#-------------------------------------------------------------------------------
# Compose rasters by maps
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Compose rasters by maps
#'
#' @description
#' Compose Rasters into zonal summaries, indicated by another raster.
#'
#' @param x S4 object. The product of interest.
#' @param y S4 object. The product determining the zones.
#' @param variablex character. The variable of product `x`.
#' @param variabley character. The variable of product `y`.
#' @param fun character or function. The function to apply for the summaries.
#' @param ... extra arguments passed to `fun`.
#'
#' @return A data.frame with the zonal summaries. The data.frame is also saved
#' in the cronus database in .csv format.
#'
#' @details Currently, `y` can only be a NASS Cropmap.
#'
#' @importFrom terra rast project zonal
#' @importFrom tidyr pivot_longer
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
#' ## Daymet
#'
#' # Create objects
#' x <- new("Daymet", region = region, date = date)
#' y <- new("Cropmaps", region = region, date = date)
#'
#' # Compose
#' a <- compose(x, y, variablex = "gdd",  variabley = "cdl_default", fun = "mean")
#' b <- compose(x, y, variablex = "prcp", variabley = "cdl_default", fun = "mean")
#'
#' ## MOD09GA
#'
#' # Create objects
#' x <- new("Mod09ga", region = region, date = date)
#' y <- new("Cropmaps", region = region, date = date)
#'
#' a <- compose(x, y, variablex = "ndvi_smooth_wt1", variabley = "cdl_recoded", fun = "mean")
#' }
setGeneric("compose", signature = c("x", "y"),
           function(x, y, ...) { standardGeneric("compose") })

#' @rdname compose
setMethod("compose",
          signature  = c(x = "Database", y = "Cropmaps"),
          definition = function(x, y, variablex, variabley, fun = "mean", ...) {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  productx <- get_product(x)
  producty <- get_product(y)

  # Get the toi
  toi <- get_toi(date)

  # Get the directories
  dir_var1 <- create_db(dir, region, product = productx, variable = variablex)
  dir_var2 <- create_db(dir, region, product = producty, variable = variabley)
  dir_var3 <- create_db(dir, region, product = productx, variable = "tseries")
  path_var1 <- file.path(dir_var1, paste0(toi$name, ".tif"))
  path_var2 <- file.path(dir_var2, paste0(toi$uyear, ".tif"))
  path_var3 <- file.path(dir_var3, paste0(variablex, ".RData"))

  # Create a progress bar object
  frm <- paste0("Composing ", variablex, " [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull")
  pb <- progress::progress_bar$new(format = frm, total = length(toi$uyear), clear = FALSE)

  # Load the rasters
  x <- list()
  for (year in toi$uyear) {
    pb$tick()
    ras_var1 <- terra::rast(path_var1[toi$year == year])
    ras_var2 <- terra::rast(path_var2[toi$uyear == year])
    ras_var2 <- terra::project(ras_var2, ras_var1, method = "near")
    y <- terra::zonal(ras_var1, ras_var2, fun = fun, na.rm = TRUE, ...)
    y <- tidyr::pivot_longer(y, cols = names(y)[-1], names_to = "pdoy", values_to = variablex)
    x[[as.character(year)]] <- y
  }

  # Tidy the data
  x <- do.call("rbind", x)
  x$Date <- pdoy_to_date(x$pdoy)
  x <- x[, c(1, 4, 3)]
  names(x) <- c("Class", "Date", variablex)
  data <- split(x, f = x$Class)
  data <- lapply(data, function(x) { x[-1] })

  # Save the data
  save(data, file = path_var3)
  invisible(data)

})
