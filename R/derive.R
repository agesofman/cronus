#-------------------------------------------------------------------------------
# Derive Data
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Derive data
#'
#' @description
#' Derive variables based on existing data.
#'
#' @param x S4 object. A product of interest.
#' @param y S4 object. A product of interest.
#' @param variable character. A function to compute the variable of interest.
#' @param varxy character. Variables used to derive `variable`.
#' @param ... extra arguments to `variable`.
#'
#' @return nothing. The data are saved directly in the cronus database.
#'
#' @export
#' @importFrom terra rast lapp
#' @importFrom tidyr pivot_longer
#' @importFrom progress progress_bar
#' @importFrom zip unzip
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
#' # Create parameters (cardinal temperatures)
#' z <- new("Parameters", region = region, date = date)
#' tb_ct <- read(z, "default")$tb_ct
#'
#' # Derive gdd (take a look at the gdd function)
#' derive(x, y, "gdd", varxy = c("tmin", "tmax", "cdl_default"), tb_ct = tb_ct)
#'
#' ## MOD09GA
#'
#' # Create objects
#' x <- new("Mod09ga", region = region, date = date)
#'
#' # Derive ndvi and cloudmask
#' derive(x, "ndvi")
#' derive(x, "cloudmask")
#' }
setGeneric("derive", signature = c("x"),
           function(x, ...) { standardGeneric("derive") })

#' @rdname derive
setMethod("derive",
          signature  = c(x = "Daymet"),
          definition = function(x, y, variable, varxy, ...) {

            # Get slots
            region <- x@region
            date <- x@date
            dir <- x@dir
            product <- get_product(x)

            # Get the toi
            toi <- get_toi(date)

            # Get the directories
            dir_var <- create_db(dir, region, product = product, variable = variable)
            files <- get_files(x, y, variable, varxy)

            # Create a progress bar object
            frm <- paste0("Deriving ", variable, " [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull")
            pb <- progress::progress_bar$new(format = frm, total = length(date), clear = FALSE)

            # Derive files
            for (i in seq_along(date)) {
              pb$tick()
              path_var <- file.path(dir_var, paste0(toi$name[i], ".tif"))
              y <- terra::rast(files[[i]])
              z <- terra::lapp(y, variable, ..., filename = path_var, overwrite = TRUE, wopt = list(names = toi$name[i]))
            }

          })

#' @rdname derive
setMethod("derive",
          signature  = c(x = "Satellite"),
          definition = function(x, variable) {

            # Get slots
            region <- x@region
            date <- x@date
            dir <- x@dir
            product <- get_product(x)

            # Get the toi
            toi <- get_toi(date)

            # Get the directories
            dir_mosaic <- create_db(dir, region, product = product, variable = "mosaic")
            dir_var <- create_db(dir, region, product = product, variable = variable)
            files <- get_files(x, variable)

            # Create a progress bar object
            frm <- paste0("Deriving ", variable, " [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull")
            pb <- progress::progress_bar$new(format = frm, total = length(date), clear = FALSE)

            # Derive files
            for (i in seq_along(date)) {
              pb$tick()
              path_mosaic <- file.path(dir_mosaic, paste0(toi$name[i], ".zip"))
              if (file.exists(path_mosaic)) {
                path_var <- file.path(dir_var, paste0(toi$name[i],".tif"))
                zip::unzip(path_mosaic, files = files, exdir = tempdir())
                y <- terra::rast(file.path(tempdir(), files))
                z <- terra::lapp(y, variable, filename = path_var, overwrite = TRUE, wopt = list(names = toi$name[i]))
              } else {
                warning("File ", path_mosaic, " does not exists. Skipping file.")
              }
            }
          })
