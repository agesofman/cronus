#-------------------------------------------------------------------------------
# Download data
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Download data
#'
#' @description
#' Download agricultural, environmental, or satellite data for a region and time
#' of interest.
#'
#' @param x S4 object. The product of interest.
#' @param variable character. The variable of interest.
#' @param ringname character. A keyring to handle the provider credentials.
#' @param ... extra arguments.
#'
#' @return For most products, nothing is returned. The data are saved directly
#' in the cronus database. For lightweight data (such as variables of the
#' Quickstats product), the downloaded data are returned.
#'
#' @details
#' Currently, the function can download:
#'  - The Quickstats product of NASS.
#'  - The Cropland Data Layer variable of NASS.
#'  - The Daymet product of ORNL.
#'  - The MOD09GA and MYD09GA products of MODIS.
#'  - The Landsat TM, ETM and 8 products.
#'  - The Sentinel-2 MSI and Sentinel-3 Synergy products.
#'
#' Details concerning the Quickstats product:
#' The function is a wrapper of the `nassqs()` function from the `rnassqs`
#' package. In order to download the data, an API key is required; information
#' can be found in the `rnassqs` package vignette.
#'
#' Details concerning the Daymet product:
#' The S4 class `Daymet` can be used to handle data of the daymet product. It is
#' a wrapper of the `get_daymet()` function from the `FedData` package.
#'
#' Details concerning the satellite sector:
#' This function uses the `rsat::rsat_search()` function to search for available
#' products in the requested dates. Then, it recursively calls
#' `rsat::rsat_download()` and `rsat::rsat_mosaic()` to download and mosaic the
#' data, before deleting the original hdf files to save disk space. The
#' satellite data are extracted in GeoTiff format (zipped).
#' The recursive call of the function has a slight performance impact, but it is
#' crucial in order to download data over large regions and periods of time.
#' For example, assume we want to download the MOD09GA product for the state of
#' Nebraska for one day. The (3) hdf files required combined surpass 250MB of
#' memory space, while the mosaiced, zipped Geotiff images take about 15MB.
#'
#' @export
#' @importFrom rnassqs nassqs_auth nassqs
#' @importFrom cdlTools getCDL
#' @importFrom terra writeRaster setGDALconfig
#' @importFrom FedData get_daymet
#' @importFrom rsat new_rtoi rsat_search rsat_download rsat_mosaic set_credentials
#'
#' @examples
#' \dontrun{
#' # Define required variables
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- date_seq("2002-01-01", "2002-12-31")
#'
#' ## Quickstats Progress
#'
#' # Create the object
#' x <- new("Quickstats", region = region, date = date)
#'
#' # Download the data
#' data <- download(x, "progress", ringname)
#' head(data$Corn)
#'
#' ## Cropmaps CDL
#'
#' # Create the object
#' x <- new("Cropmaps", region = region, date = date)
#'
#' # Download the data
#' download(x, "cdl")
#'
#' ## Daymet
#'
#' # Create the object
#' x <- new("Daymet", region = region, date = date)
#'
#' # Download the data
#' download(x, c("tmin", "tmax"))
#'
#' ## MOD09GA
#'
#' # Create the object
#' x <- new("Mod09ga", region = region, date = date)
#'
#' # Download the data
#' download(x, ringname)
#' }
setGeneric("download", signature = c("x"),
           function(x, ...) { standardGeneric("download") })

#' @rdname download
setMethod("download",
          signature  = c(x = "Quickstats"),
          definition = function(x, variable, ringname = NULL) {

  # NASS key
  if (is.null(ringname)) {
    stop("Keyring not provided. See ?create_account for more information.")
  }
  rnassqs::nassqs_auth(key = get_password(ringname, "nass"))

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)

  # Get toi
  toi <- get_toi(date)

  # Get directories
  dir_var <- create_db(dir = dir, region = region, product = product, variable = variable)
  path_var <- file.path(dir_var, paste0(variable, ".RData"))

  # Parameters to query on
  params <- list(source_desc       = "SURVEY",
                 sector_desc       = "CROPS",
                 agg_level_desc    = "STATE",
                 state_name        = toupper(unname(region@name)),
                 year              = toi$uyear,
                 statisticcat_desc = toupper(variable))

  # Download the data
  message("Downloading the data")
  data <- rnassqs::nassqs(params)

  # Tidy the data
  data <- do.call(paste0("tidy_", tolower(variable)), list(data = data))

  # Save the data
  save(data, file = path_var)
  invisible(data)

})

#' @rdname download
setMethod("download",
          signature  = c(x = "Cropmaps"),
          definition = function(x, variable = NULL) {

  if (is.null(variable)) {
    variable <- c("cdl")
  }

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)

  # Get the toi
  toi <- get_toi(date)

  # Allow for auxiliary files and colors
  terra::setGDALconfig("GDAL_PAM_ENABLED", "TRUE")

  # Create the directories
  path <- create_db(dir, region, product = product, variable = variable)
  dir_temp <- tempdir()

  # Download CDL
  message("Downloading the Cropland Data Layers")
  for (year in toi$uyear) {
    cat(" Year", year, "\n")
    cdlTools::getCDL(region@name, year, location = dir_temp)
    ls_cdl <- list.files(dir_temp, pattern = paste0(year, ".*\\.tif$"),
                         full.names = TRUE)
    cdl <- terra::rast(ls_cdl)
    df_cat <- terra::cats(cdl)[[1]][, c(1, 5, 2, 3, 4, 6)]
    terra::set.cats(cdl, value = df_cat)
    filename <- file.path(path, paste0(year, ".tif"))
    terra::writeRaster(cdl, filename = filename, overwrite = TRUE,
                       datatype = "INT1U", names = year)
  }

})

#' @rdname download
setMethod("download",
          signature  = c(x = "Daymet"),
          definition = function(x, variable = NULL) {

  if (is.null(variable)) {
    variable <- c("tmax", "tmin", "prcp", "dayl", "srad", "swe", "vp")
  }

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)

  # Region of interest
  roi <- get_roi(region)
  toi <- get_toi(date)
  n <- length(variable)
  m <- length(toi$uyear)

  # Create the directories
  path <- create_db(dir, region, product = product, variable = variable)

  # Create a progress bar object
  frm <- "Downloading [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull"
  pb <- progress::progress_bar$new(format = frm, total = n * m, clear = FALSE)

  # Variable loop
  for (i in 1:n) {
    # Year loop
    for (j in toi$uyear) {

      pb$tick()
      k <- (toi$year == j)

      # Download the data
      suppressMessages(
        data <- FedData::get_daymet(template = roi,
                                    label = region@name,
                                    elements = variable[i],
                                    years = j,
                                    force.redo = TRUE)[[1]]
      )

      # Save the rasters
      filename <- file.path(path[i], toi$name[k])
      terra::setGDALconfig("GDAL_PAM_ENABLED", "FALSE")
      raster::writeRaster(data[[k]],
                          filename = filename,
                          format = "GTiff",
                          bylayer = TRUE,
                          overwrite = TRUE)
    }
  }

})

#' @title Create an rtoi object
#'
#' @description This function creates an rtoi object for a specific region,
#' according to the rsat package.
#'
#' @param region an object of class Region. The region of interest.
#'
#' @return A data.frame with the requested data.
#'
#' @details
#' `create_rtoi()` is a wrapper function, calling the `new_rtoi()` function from
#' the `rsat` package. Both the `db_path` and `rtoi_path` are created inside a
#' temporary directory, using `tempdir()`.
#'
#' @seealso [rsat::new_rtoi()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_rtoi("nebraska")
#' }
create_rtoi <- function(region) {

  name <- "satellite"
  rtoi_path <-file.path(tempdir(), "rsat_rtoi")
  db_path <- file.path(tempdir(), "rsat_db")
  dir.create(rtoi_path, showWarnings = FALSE, recursive = FALSE)
  dir.create(db_path, showWarnings = FALSE, recursive = FALSE)
  file <- file.path(rtoi_path, name, paste0(name, ".rtoi"))

  if (file.exists(file)) {
    unlink(file)
  }

  rsat::new_rtoi(name = name,
                 region = get_roi(region),
                 rtoi_path = rtoi_path,
                 db_path = db_path)

}

#' @rdname download
setMethod("download",
          signature  = c(x = "Satellite"),
          definition = function(x, ringname = NULL) {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)
  variable <- "mosaic"

  # Create an rtoi
  rtoi <- create_rtoi(region)
  path_cronus <- create_db(dir, region, product = product, variable = variable)
  path_rtoi <- file.path(tempdir(), "rsat_rtoi", "satellite")

  # Satellite key
  check_keyring(ringname)
  provider <- get_branch(product = product)$provider
  for (service in get_service(provider)) {
    rsat::set_credentials(get_username(ringname, service),
                          get_password(ringname, service), service)
  }

  # Add available files to rtoi
  rsat::rsat_search(region = rtoi, product = toupper(product), dates = date)

  # Get rtoi information
  # dir_database <- rsat::get_database(rtoi)
  # rtoi_records <- rsat::records(rtoi)
  # records_dates <- rsat::records(rtoi)@date
  # unique_dates <- unique(records_dates)
  # records_paths <- file.path(dir_database, rtoi_records@file_path)

  # Download files
  # for (i in 1:length(unique_dates)) {
    # index <- records_dates %in% unique_dates#[i]
    # rsat::rsat_download(x = rtoi_records[index], out.dir = dir_database)
    rsat::rsat_download(rtoi)
    rsat::rsat_mosaic(rtoi)
    #unlink(records_paths[index])
    path_zip <- list.files(path_rtoi, pattern = "*.zip$", recursive = TRUE,
                           full.names = TRUE)
    file.copy(from = path_zip, to = path_cronus, recursive = TRUE)
    unlink(path_zip)
  #}

})
