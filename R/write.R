#-------------------------------------------------------------------------------
# Write files
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Write metadata
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Write metadata files in the cronus database.
#'
#' @param x S4 object. A product of interest.
#' @param name character. The name of the metadata files.
#' @param df_cat data.frame A data frame holding the cdl categories.
#' @param tb_rcl matrix A matrix holding the cdl value recoding.
#' @param tb_ct matrix. A matrix holding the cardinal temperatures.
#' @param ... extra arguments.
#'
#' @return nothing. The data are saved directly in the cronus database.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- date_seq("2002-01-01", "2002-12-31")
#' dir <- getwd()
#'
#' x <- new("Parameters", region = region, date = date, dir = dir)
#' # tb_ct is a matrix with cardinal temperatures
#' write_metadata(x, name = "default", tb_ct = tb_ct)
#' }
setGeneric("write_metadata", signature = c("x"),
           function(x, ...) { standardGeneric("write_metadata") })

#' @rdname write_metadata
setMethod("write_metadata",
          signature  = c(x = "Cropmaps"),
          definition = function(x, name, df_cat = NULL, tb_rcl = NULL) {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)
  variable <- "metadata"

  # Get the directories
  dir_meta <- create_db(dir, region, product = product, variable = variable)

  # Categories data frame
  if (!is.null(df_cat)) {
    path_cat <- file.path(dir_meta, paste0("cat_", name, ".csv"))
    write.csv(df_cat, file = path_cat)
  }
  # Values table
  if (!is.null(tb_rcl)) {
    path_rcl <- file.path(dir_meta, paste0("rcl_", name, ".txt"))
    write.table(tb_rcl, file = path_rcl)
  }

})

#' @rdname write_metadata
setMethod("write_metadata",
          signature  = c(x = "Parameters"),
          definition = function(x, name, tb_ct = NULL) {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)

  # Cardinal temperatures table
  if (!is.null(tb_ct)) {
    dir_ct <- create_db(dir, region, product = product, variable = "ct")
    path_ct <- file.path(dir_ct, paste0("ct_", name, ".txt"))
    write.table(tb_ct, file = path_ct)
  }

})
