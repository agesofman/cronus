#-------------------------------------------------------------------------------
# Handle .RProfile
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Get .RProfile path
#'
#' @description .RProfile is a file loaded at the startup of each R session.
#' It can be modified to contain code, so that the user does not have to
#' explicitely execute the same commands every single time.
#'
#' @param prof character. The type of profile, one of 'user' or 'project'.
#'
#' @details There are multiple locations in which .RProfile files are stored.
#' This function can only modify the 'user' and the 'project' .RProfile files.
#' Here, 'project' refers to an RStudio project. The examples show all possible
#' locations of the .RProfile files. See also \link[base]{Startup}.
#'
#' @return character. The .RProfile file path.
#'
#' @importFrom rprojroot find_rstudio_root_file
#' @export
#'
#' @seealso [base::Startup], [rprojroot::find_rstudio_root_file()]
#'
#' @examples
#' \dontrun{
#' get_path_prof("user")
#' get_path_prof("project")
#'
#' # Locations of .RProfile files in loading order
#' Sys.getenv("R_PROFILE")
#' file.path(Sys.getenv("R_HOME"), "etc", "Rprofile.site")
#' Sys.getenv("R_PROFILE_USER")
#' file.path(getwd(), ".Rprofile")
#' file.path(Sys.getenv("HOME"), ".Rprofile") # this is selected by 'prof = user'.
#' }
get_path_prof <- function(prof) {

  if (prof == "user") {
    file.path(Sys.getenv("HOME"), ".Rprofile")
  } else if (prof == "project") {
    file.path(rprojroot::find_rstudio_root_file(), ".Rprofile")
  } else {
    stop("prof should be one of 'user' or 'project', got ", prof)
  }

}

#' @title Edit .RProfile
#'
#' @description These functions can help the user edit an .RProfile file
#' without actually opening the file.
#'
#' @param newline character. Line to add in the file.
#' @param variable character. Variable to add in `options(variable = value)`.
#' @param value character. Value to add in `options(variable = value)`..
#' @param prof character. The type of profile, one of 'user' or 'project'.
#'
#' @describeIn edit_prof_line Add a line of code in the .RProfile file.
#'
#' @return nothing. The .RProfile file is edited.
#' @export
#'
#' @seealso [cronus::set_path_demeter()]
#'
#' @examples
#' \dontrun{
#' edit_prof_line("library(agesofman)", prof = "user")
#' edit_prof_options("path_demeter", getwd(), prof = "user")
#' }
edit_prof_line <- function(newline, prof = "user") {

  # Get profile path
  path_prof <- get_path_prof(prof)

  # Read Rprofile
  file_prof <- readLines(path_prof)

  # Add line
  file_prof <- file_prof[-grep(newline, file_prof)]
  file_prof <- c(file_prof, newline)

  # Write file
  writeLines(file_prof, con = path_prof)

}

#' @describeIn edit_prof_line Add a call to `options()` in the .RProfile file.
#' @export
edit_prof_options <- function(variable, value, prof = "user") {

  # Get profile path
  path_prof <- get_path_prof(prof)

  # Read Rprofile
  file_prof <- readLines(path_prof)

  # Remove old variable
  line_old <- paste0("options\\(", variable, ".*\\)$")
  line_old_index <- grep(line_old, file_prof)
  if (length(line_old_index) > 0) {
    file_prof <- file_prof[!line_old_index]
  }

  # Add new variable
  line_new <- paste0("options(", variable, " = '", value, "')")
  file_prof <- c(file_prof, line_new)

  # Write file
  writeLines(file_prof, con = path_prof)

}

#' @title Set Demeter path
#'
#' @description Set a default path_demeter by editing the .RProfile file.
#'
#' @param path character. The path to the database directory.
#' @param prof character. The type of profile, one of 'user' or 'project'.
#'
#' @return nothing. The .RProfile file is edited.
#' @export
#'
#' @seealso [cronus::edit_prof_options()]
#'
#' @examples
#' \dontrun{
#' set_path_demeter(getwd(), prof = "user")
#' get_path_demeter()
#' }
set_path_demeter <- function(path, prof = "user") {
  edit_prof_options("path_demeter", path, prof)
}

#' @rdname set_path_demeter
#' @export
get_path_demeter <- function() {
  getOption("path_demeter")
}
