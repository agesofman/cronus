#-------------------------------------------------------------------------------
# Handle .RProfile
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Handle .Rprofile and .Renviron
#'
#' @description .Rprofile and .Renviron are files loaded at the start-up of each
#' R session. They can be modified to contain code and variables respectively,
#' so that users do not have to explicitly execute the same commands every
#' single time. These functions can help the users edit .Rprofile and .Renviron
#' files without actually opening them.
#'
#' @param type character. The type, one of 'environ' or 'profile'.
#' @param newline character. Line to add in the file.
#' @param oldline character. Line to remove before adding the new one.
#'
#' @details There are multiple locations in which .Rprofile and .Renviron files
#' are stored. This function can only modify the 'user' files. The examples
#' show all possible locations of the startup files. See also
#' \link[base]{Startup}.
#'
#' Argument `oldline` is used to remove duplicate lines of code. It can be
#' specified as a regex to look for complex code expressions and remove them.
#' The examples show one such instance.
#'
#' @describeIn get_startup_path Returns the .Renviron or .Rprofile file path.
#'
#' @export
#'
#' @seealso [base::Startup], [base::options()], [cronus::set_path_demeter()]
#'
#' @examples
#' \dontrun{
#' # Locations of .Rprofile files in loading order
#' Sys.getenv("R_PROFILE")
#' file.path(Sys.getenv("R_HOME"), "etc", "Rprofile.site")
#' Sys.getenv("R_PROFILE_USER")
#' file.path(getwd(), ".Rprofile")
#' file.path(Sys.getenv("HOME"), ".Rprofile") # 'user' file
#'
#' # Locations of .Renviron files in loading order
#' Sys.getenv("R_ENVIRON")
#' file.path(Sys.getenv("R_HOME"), "etc", "Renviron.site")
#' Sys.getenv("R_ENVIRON_USER")
#' file.path(getwd(), ".Renviron")
#' file.path(Sys.getenv("HOME"), ".Renviron") # 'user' file
#'
#' # Get the path
#' type <- "environ"
#' get_startup_path(type = type)
#'
#' # Edit the file
#' newline1 <- "my_variable=1"
#' newline2 <- "my_variable=2"
#' add_startup_line(newline1, type = type)
#' file_startup_1 <- read_startup(type)
#' add_startup_line(newline2, oldline = 'my_variable=', type = type)
#' file_startup_2 <- read_startup(type)
#'
#' # Use the package usethis to edit the files directly
#' library(usethis)
#' edit_r_profile(scope = "user")
#' edit_r_environ(scope = "user")
#' }
get_startup_path <- function(type = "environ") {

  if (type == "environ") {
    file.path(Sys.getenv("HOME"), ".Renviron")
  } else if (type == "profile") {
    file.path(Sys.getenv("HOME"), ".Rprofile")
  } else {
    stop("type should be one of 'environ' or 'profile', got ", type)
  }

}

#' @describeIn get_startup_path Read the .Renviron or .Rprofile file.
#' @export
read_startup <- function(type = "environ") {

  # Get path
  path_startup <- get_startup_path(type)

  # Create file if it does not exist
  if (!file.exists(path_startup)) {
    file.create(path_startup)
  }

  # Read file
  readLines(path_startup)

}

#' @describeIn get_startup_path Remove a line of code from the .Renviron or .Rprofile file.
#' @export
remove_startup_line <- function(oldline, type = "environ") {

  # Get path
  path_startup <- get_startup_path(type)

  # Read Rprofile
  file_startup <- read_startup(type)

  # Add line
  oldline_index <- grep(oldline, file_startup)
  if (length(oldline_index) > 0) {
    file_startup <- file_startup[-oldline_index]
  }

  # Write file
  writeLines(file_startup, con = path_startup)

}

#' @describeIn get_startup_path Add a line of code from the .Renviron or .Rprofile file.
#' @export
add_startup_line <- function(newline, oldline = newline, type = "environ") {

  # Get profile path
  path_startup <- get_startup_path(type)

  # Remove in case of duplicates
  remove_startup_line(oldline, type)

  # Read Rprofile
  file_startup <- read_startup(type)

  # Add line
  file_startup <- c(file_startup, newline)

  # Write file
  writeLines(file_startup, con = path_startup)

}

#' @title Set Demeter path
#'
#' @description Set or get the default path_demeter by editing the .Renviron
#' file.
#'
#' @param path character. The path to the database directory.
#'
#' @return nothing. The .Renviron file is edited.
#' @export
#'
#' @seealso [cronus::get_startup_path()]
#'
#' @examples
#' \dontrun{
#' set_path_demeter(getwd())
#' get_path_demeter()
#' }
set_path_demeter <- function(path) {
  newline <- paste0("path_demeter='", path, "'")
  oldline <- "path_demeter="
  add_startup_line(newline, oldline = oldline, type = "environ")
}

#' @rdname set_path_demeter
#' @export
get_path_demeter <- function() {
  path_renviron <- cronus::get_startup_path(type = "environ")
  path_demeter <- NULL
  if (file.exists(path_renviron)) {
    source(path_renviron, local = TRUE)
  }
  path_demeter
}
