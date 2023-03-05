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

#' @title Read scripts
#'
#' @description This set of functions sources scripts inside the structured
#' directory of the `hermes` project and returns an appropriate object. These
#' functions are used in the model creation process.
#'
#' @param name character. The name of the script (without the .R suffix).
#' @param project character. The project the script concerns.
#' @param dir character. The hermes project directory.
#'
#' @describeIn read_exe source the script.
#'
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
#' # Read files
#' read_exe("test", "persephone", dir = path_hermes)
#' prm <- read_prm("test", "persephone", dir = path_hermes)
#' }
read_exe <- function(name, project, dir = get_path_hermes()) {
  path <- file.path(dir, "projects", project, "exe", paste0(name, ".R"))
  source(path)
}
