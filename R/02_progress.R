#-------------------------------------------------------------------------------
# Progress Class
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Progress Class
#'
#' @description An S3 class that contains the necessary information to study
#' the large-scale progress of the crops, expressed as the crop percentage that
#' occupies each phenological stage over an area of interest.
#'
#' @details The `data.frame()` must include the following columns:
#'
#' - Crop (`character`)
#' - Stage (`factor`)
#' - Season (`numeric`)
#' - Time (`numeric`)
#' - Date (`Date`)
#' - Percentage (`numeric`)
#' - CumPercentage (`numeric`)
#'
#' @param x data.frame. The object to gain the class of interest. See Details.
#' @param i integer. The index passed in `[`.
#'
#' @return An object of class `Progress` or `ProgressList`.
#'
#' @importFrom lubridate dmy
#' @export
#'
#' @examples \dontrun{
#' # Simple initialization
#' x <- Progress()
#' class(x)
#' x <- ProgressList()
#' class(x)
#' class(x[[1]])
#'
#' # Define required variables
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- date_seq("2002-01-01", "2002-12-31")
#'
#' # Quickstats Progress
#'
#' # Create the object
#' x <- new("Quickstats", region = region, date = date)
#'
#' # Download the data
#' data <- download(x, "progress", ringname)
#' class(data)
#' class(data[["Corn"]])
#' head(data$Corn)
#'
#' # Plot the data
#' plot(data, crops = "Winter Wheat", year = 2021)
#' }
new_Progress <- function(x = NULL) {
  if (is.null(x)) {
    x <- data.frame(Crop = character(),
                    Stage = factor(),
                    Season = numeric(),
                    Time = numeric(),
                    Date = lubridate::dmy(),
                    CumPercentage = numeric())
  }
  stopifnot(is.data.frame(x))
  structure(x, class = c("Progress", "data.frame"))
}

#' @rdname new_Progress
#' @export
validate_Progress <- function(x) {
  vars <- c("Crop", "Stage", "Season", "Time", "Date")
  if (!all((vars %in% names(x)))) {
    stop("Object provided does not meet the requirements to be classified as
         Progress. Should contain columns ", vars, ", instead got ", names(x))
  } else {
    if (!is.character(x$Crop)) {
      stop("Column Crop must be character, instead got ", class(x$Crop))
    }
    if (!is.factor(x$Stage)) {
      stop("Column Stage must be factor, instead got ", class(x$Stage))
    }
    if (!is.numeric(x$Season)) {
      stop("Column Season must be numeric, instead got ", class(x$Season))
    }
    if (!is.numeric(x$Time)) {
      stop("Column Time must be numeric, instead got ", class(x$Time))
    }
    if (!inherits(x$Date, 'Date')) {
      stop("Column Date must be Date, instead got ", class(x$Date))
    }
    if (!is.numeric(x$CumPercentage)) {
      stop("Column CumPercentage must be numeric, instead got ", class(x$CumPercentage))
    }
  }
  x
}

#' @rdname new_Progress
#' @export
Progress <- function(x = NULL) {
  validate_Progress(new_Progress(x))
}

#' @rdname new_Progress
#' @export
new_ProgressList <- function(x = NULL) {
  if (is.null(x)) {
    x <- list(new_Progress())
  }
  stopifnot(is.list(x))
  for(i in seq_along(x)) {
    x[[i]] <- Progress(x[[i]])
  }
  structure(x, class = c("ProgressList", "list"))
}

#' @rdname new_Progress
#' @export
validate_ProgressList <- function(x) {
  lapply(x, validate_Progress)
  x
}

#' @rdname new_Progress
#' @export
ProgressList <- function(x = NULL) {
  validate_ProgressList(new_ProgressList(x))
}

#' @rdname new_Progress
#' @export
`[.ProgressList` <- function(x, i) {
  new_ProgressList(NextMethod())
}

setOldClass("Progress")
setOldClass("ProgressList")
