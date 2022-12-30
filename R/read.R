#-------------------------------------------------------------------------------
# Read files
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Read data
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Read data stored in the cronus database.
#'
#' @param x S4 object. A product of interest.
#' @param variable character. The variable of interest.
#' @param name character. The name of the metadata files.
#' @param df_var data.frame. A data frame with columns Product and Variable.
#' @param ... extra arguments.
#'
#' @return The data of interest (list, data.frame, raster, or other).
#'
#' @export
#' @importFrom dplyr left_join
#'
#' @examples
#' \dontrun{
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- date_seq("2002-01-01", "2002-12-31")
#' dir <- getwd()
#'
#' x <- new("Quickstats", region = region, date = date, dir = dir)
#' variable <- "progress"
#' list_progress <- read(x, variable)
#'
#' x <- new("Parameters", region = region, date = date, dir = dir)
#' read(x, name = "default")
#' }
setGeneric("read", signature = c("x"),
           function(x, ...) { standardGeneric("read") })

#' @rdname read
setMethod("read",
          signature  = c(x = "Quickstats"),
          definition = function(x, variable) {

  # Get slots
  region <- x@region
  dir <- x@dir
  product <- get_product(x)

  # Get the directories
  dir_var <- create_db(dir = dir, region = region, product = product, variable = variable)
  path_var <- file.path(dir_var, paste0(variable, ".RData"))

  # Load the data
  load(path_var)
  data

})

#' @rdname read
setMethod("read",
          signature  = c(x = "Cropmaps"),
          definition = function(x, name) {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)
  variable <- "metadata"
  md <- list()

  # Get the directories
  dir_meta <- create_db(dir, region, product = product, variable = variable)
  path_cat <- file.path(dir_meta, paste0("cat_", name, ".csv"))
  path_rcl <- file.path(dir_meta, paste0("rcl_", name, ".txt"))

  # Read the files
  # Categories data frame
  if (file.exists(path_cat)) {
    md$df_cat <- read.csv(path_cat)
  }
  # Values table
  if (file.exists(path_rcl)) {
    path_rcl <- file.path(dir_meta, paste0("rcl_", name, ".txt"))
    md$tb_rcl <- read.table(path_rcl)
  }

  # Return the files
  md

})

#' @rdname read
setMethod("read",
          signature  = c(x = "Parameters"),
          definition = function(x, name) {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)
  md <- list()

  # Get the directories
  dir_ct <- create_db(dir, region, product = product, variable = "ct")
  path_ct <- file.path(dir_ct, paste0("ct_", name, ".txt"))

  # Read the files
  # Cardinal temperatures table
  if (file.exists(path_ct)) {
    md$tb_ct <- read.table(path_ct)
  }

  # Return the files
  md

})

#' @rdname read
setMethod("read",
          signature  = c(x = "Daymet"),
          definition = function(x, variable) {

  # Get slots
  region <- x@region
  dir <- x@dir
  product <- get_product(x)

  # Get the directories
  dir_var <- create_db(dir = dir, region = region, product = product, variable = "tseries")
  path_var <- file.path(dir_var, paste0(variable, ".RData"))

  # Load the data
  load(path_var)
  data

})

#' @rdname read
setMethod("read",
          signature  = c(x = "Mod09ga"),
          definition = function(x, variable) {

  # Get slots
  region <- x@region
  dir <- x@dir
  product <- get_product(x)

  # Get the directories
  dir_var <- create_db(dir = dir, region = region, product = product, variable = "tseries")
  path_var <- file.path(dir_var, paste0(variable, ".RData"))

  # Load the data
  load(path_var)
  data

})

#' @rdname read
setMethod("read",
          signature  = c(x = "Database"),
          definition = function(x, df_var) {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  list_data <- list()

  x <- new(df_var$Product[1], region = region, date = date, dir = dir)
  list_data <- read(x, df_var$Variable[1])

  for (i in 2:dim(df_var)[1]) {
    x <- new(df_var$Product[i], region = region, date = date, dir = dir)
    list2 <- read(x, df_var$Variable[i])[names(list_data)]
    list_data <- mapply(dplyr::left_join, list_data, list2, by = "Date", SIMPLIFY = FALSE)
  }

  # Return the data
  list_data

})
