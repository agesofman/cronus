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

#' @title Save and read objects
#'
#' @description This set of functions saves and reads files inside the `hermes`
#' structured directory.
#'
#' @param object ANY. The object to save / load.
#' @param name character. The name of the script (without the .RData suffix).
#' @param project character. The project the script concerns.
#' @param dir character. The hermes project directory.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_path_hermes_obj("objectname", "projectname")
#'
#' x <- 1:3
#' save_obj(x, "objectname", "projectname")
#' read_obj("objectname", "projectname")
#' }
get_path_hermes_obj <- function(name, project, dir = get_path_hermes()) {
  dir_output <- file.path(dir, project, "obj")
  dir.create(dir_output, showWarnings = FALSE, recursive = TRUE)
  file.path(dir_output, paste0(name, ".RData"))
}

#' @rdname get_path_hermes_obj
#' @export
save_obj <- function(object, name, project, dir = get_path_hermes()) {
  save(object, file = get_path_hermes_obj(name, project, dir))
}

#' @rdname get_path_hermes_obj
#' @export
read_obj <- function(name, project, dir = get_path_hermes()) {
  object <- NULL
  load(file = get_path_hermes_obj(name, project, dir))
  object
}
