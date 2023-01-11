#-------------------------------------------------------------------------------
# Handle .RProfile
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Handle .Rprofile and .Renviron
#'
#' @description .Rrofile and .Renviron are files loaded at the start-up of each
#' R session. They can be modified to contain code and variables respectively,
#' so that users do not have to explicitly execute the same commands every
#' single time. These functions can help the users edit .Rprofile and .Renviron
#' files without actually opening them.
#'
#' @param level character. The level of file, one of 'user' or 'project'.
#' @param type character. The type, one of 'environ' or 'profile'.
#' @param newline character. Line to add in the file.
#' @param oldline character. Line to remove before adding the new one.
#'
#' @details There are multiple locations in which .Rprofile and .Renviron files
#' are stored. This function can only modify the 'user' and the 'project' files.
#' Here, 'project' refers to an RStudio project. The examples show all possible
#' locations of the startup files. See also \link[base]{Startup}.
#'
#' Argument `oldline` is used to remove duplicate lines of code. It can be
#' specified as a regex to look for complex code expressions and remove them.
#' The examples show one such instance.
#'
#' @describeIn get_startup_path Returns the .Renviron or .Rprofile file path.
#'
#' @importFrom rprojroot find_rstudio_root_file
#' @export
#'
#' @seealso [base::Startup], [base::options()],
#' [rprojroot::find_rstudio_root_file()], [cronus::set_path_demeter()]
#'
#' @examples
#' \dontrun{
#' # Locations of .Rprofile files in loading order
#' Sys.getenv("R_PROFILE")
#' file.path(Sys.getenv("R_HOME"), "etc", "Rprofile.site")
#' Sys.getenv("R_PROFILE_USER")
#' file.path(getwd(), ".Rprofile")
#' file.path(Sys.getenv("HOME"), ".Rprofile") # this is selected by 'level = user'.
#'
#' # Locations of .Renviron files in loading order
#' Sys.getenv("R_ENVIRON")
#' file.path(Sys.getenv("R_HOME"), "etc", "Renviron.site")
#' Sys.getenv("R_ENVIRON_USER")
#' file.path(getwd(), ".Renviron")
#' file.path(Sys.getenv("HOME"), ".Renviron") # this is selected by 'level = user'.
#'
#' # Get the path
#' level <- "user"
#' type <- "environ"
#' get_startup_path(level = level, type = type)
#'
#' # Edit the file
#' newline1 <- "my_variable=1"
#' newline2 <- "my_variable=2"
#' add_startup_line(newline1, level = level, type = type)
#' file_startup_1 <- read_startup(level = level, type = type)
#' add_startup_line(newline2, oldline = 'my_variable=', level = level, type = type)
#' file_startup_2 <- read_startup(level = level, type = type)
#'
#' # Use the package usethis to edit the files directly
#' library(usethis)
#' edit_r_profile(scope = "user")
#' edit_r_environ(scope = "user")
#' }
get_startup_path <- function(level, type = "environ") {

  if (type == "environ") {
    if (level == "user") {
      file.path(Sys.getenv("HOME"), ".Renviron")
    } else if (level == "project") {
      file.path(rprojroot::find_rstudio_root_file(), ".Renviron")
    } else {
      stop("level should be one of 'user' or 'project', got ", level)
    }
  } else if (type == "profile") {
    if (level == "user") {
      file.path(Sys.getenv("HOME"), ".Rprofile")
    } else if (level == "project") {
      file.path(rprojroot::find_rstudio_root_file(), ".Rprofile")
    } else {
      stop("level should be one of 'user' or 'project', got ", level)
    }
  } else {
    stop("type should be one of 'environ' or 'profile', got ", type)
  }

}

#' @describeIn get_startup_path Read the .Renviron or .Rprofile file.
#' @export
read_startup <- function(level = "user", type = "environ") {

  # Get path
  path_startup <- get_startup_path(level, type)

  # Create file if it does not exist
  if (!file.exists(path_startup)) {
    file.create(path_startup)
  }

  # Read file
  readLines(path_startup)

}

#' @describeIn get_startup_path Remove a line of code from the .Renviron or .Rprofile file.
#' @export
remove_startup_line <- function(oldline, level = "user", type = "environ") {

  # Get path
  path_startup <- get_startup_path(level, type)

  # Read Rprofile
  file_startup <- read_startup(level)

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
add_startup_line <- function(newline, oldline = newline, level = "user", type = "environ") {

  # Get profile path
  path_startup <- get_startup_path(level, type)

  # Remove in case of duplicates
  remove_startup_line(oldline, level)

  # Read Rprofile
  file_startup <- read_startup(level)

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
#' @param level character. The level of profile, one of 'user' or 'project'.
#'
#' @return nothing. The .RPenviron file is edited.
#' @export
#'
#' @seealso [cronus::get_startup_path()]
#'
#' @examples
#' \dontrun{
#' set_path_demeter(getwd(), level = "user")
#' get_path_demeter()
#' }
set_path_demeter <- function(path, level = "user") {
  newline <- paste0("path_demeter='", path, "'")
  oldline <- "path_demeter="
  add_startup_line(newline, oldline = oldline, level = level, type = "environ")
}

#' @rdname set_path_demeter
#' @export
get_path_demeter <- function() {
  Sys.getenv("path_demeter")
}
