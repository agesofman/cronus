#-------------------------------------------------------------------------------
# Work with hermes
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Download project `hermes`
#'
#' @description Download project `hermes` from the github repository.
#'
#' @param dir character. The parent directory inside which the project will
#' be downloaded
#'
#' @return nothing. The project is downloaded in the specified directory.
#'
#' @importFrom usethis use_course
#' @export
#'
#' @examples
#' \dontrun{
#' # Set path
#' path_hermes <- getwd()
#'
#' # Download project
#' download_hermes(path_hermes)
#'
#' # Create files object
#' files <- PersephoneFiles(prm = "test", dts = "test")
#'
#' # Create model object
#' object <- create(files)
#' }
download_hermes <- function(dir = getwd()) {
  url <- 'https://github.com/agesofman/hermes/archive/master.zip'
  usethis::use_course(url = url, destdir = dir)
}

#' @title Create a subdirectory in hermes
#'
#' @param region Region. A region of interest.
#' @param project character. The project the script concerns (e.g. cronus).
#' @param type character. The type of the object (e.g. plot, report).
#' @param class character. The class of the object.
#' @param name character. The name of the script (without the suffix).
#' @param suffix character. The name of the suffix (starting with a dot).
#' @param dir character. The hermes project directory.
#'
#' @return The absolute path is returned invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define required variables
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#'
#' create_dir_hermes(region, "cronus", "plot", "Progress", "myplot", ".pdf")
#' }
create_dir_hermes <- function(region = NULL,
                              project = NULL,
                              type = NULL,
                              class = NULL,
                              name = NULL,
                              suffix = ".RData",
                              dir = get_path_hermes())  {

  path <- file.path(dir, region@name, project, type, class)
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  if (!is.null(name)) {
    path <- file.path(path, paste0(name, suffix))
  }

  invisible(path)

}

#' @title Save to and read from hermes
#'
#' @description
#' Save an object to the `hermes` database with `save_hermes()` and easily
#' retrieve it with `read_hermes()`.
#'
#' @param object ANY. The object to save / read.
#' @param ... extra arguments
#'
#' @return `save_hermes()` returns the absolute path in which the object is
#' saved is returned invisibly. `read_hermes()` returns the object of interest.
#'
#' @export
setGeneric("save_hermes", signature = c("object"),
           function(object, ...) { standardGeneric("save_hermes") })

#' @rdname save_hermes
#' @export
setGeneric("read_hermes", signature = c("object"),
           function(object, ...) { standardGeneric("read_hermes") })
