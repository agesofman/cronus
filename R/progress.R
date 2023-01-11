#-------------------------------------------------------------------------------
# Handle Quickstats progress data
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Handle Quickstats progress data
#'
#' @description `r lifecycle::badge("stable")`
#' Tidy the Quickstats progress data.
#'
#' @param data data.frame or list. The quickstats progress variable.
#' @param cum logical. Calculate the cumulative percentage? If FALSE,
#' calculate the simple percentage.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data
#'
#' @return data.frame or list.
#'
#' @details
#' This function relies on the `tidyverse` package to tidy the data.
tidy_progress <- function(data) {

  # Bind global variables
  year <- week_ending <- commodity_desc <- short_desc <- Value <- first_week <- NULL
  Season <- Date <- Crop <- Stage <- CumPercentage <- Percentage <- Time <- NULL

  # Keep only the necessary variables
  data <- data %>%
    dplyr::select(year, week_ending, commodity_desc, short_desc, Value) %>%
    dplyr::rename(Season = year,
                  Date = week_ending,
                  Crop = commodity_desc,
                  Stage = short_desc,
                  CumPercentage = Value) %>%
    dplyr::mutate(Season = as.numeric(Season),
                  Date = as.Date(Date),
                  CumPercentage = as.numeric(CumPercentage) / 100,
                  Crop = rename_crops(Crop),
                  Stage = ifelse(Crop == "Alfalfa",
                                 paste(stringr::word(Stage, 3), "Harvest"),
                                 stringr::word(Stage, -1, sep = "PCT ")),
                  Stage = stringr::str_to_title(Stage))

  # Add initial week for each stage (CumPercentage = 0)
  data_init <- data %>%
    dplyr::group_by(Season, Crop, Stage) %>%
    dplyr::summarise(Date = min(Date) - 7, .groups = "keep") %>%
    dplyr::mutate(CumPercentage = 0) %>%
    dplyr::ungroup()

  # Add final week for each stage (CumPercentage = 1)
  data_final <- data %>%
    dplyr::group_by(Season, Crop, Stage) %>%
    dplyr::summarise(Date = max(Date) + 7, .groups = "keep") %>%
    dplyr::mutate(CumPercentage = 1) %>%
    dplyr::ungroup()

  data <- dplyr::bind_rows(data_init, data, data_final)

  # Add time step
  data <- data %>%
    dplyr::group_by(Season, Crop) %>%
    dplyr::mutate(first_week = min(Date),
                  Time = as.numeric(Date - min(Date)) / 7 + 1) %>%
    dplyr::ungroup()

  # Complete the data (so that all stages start and end at the same week)
  data <- data %>%
    dplyr::group_by(Crop) %>%
    tidyr::complete(Stage, tidyr::nesting(Season, first_week), Time) %>%
    dplyr::group_by(Crop, Stage, Season) %>%
    dplyr::arrange(Stage, Season, Time) %>%
    tidyr::fill(CumPercentage, .direction = "downup") %>%
    dplyr::ungroup()

  # Add dates
  data <- data %>%
    dplyr::group_by(Crop) %>%
    dplyr::mutate(Date = first_week + 7 * (Time - 1)) %>%
    dplyr::arrange(Crop, Stage, Season, Time) %>%
    dplyr::ungroup()

  # Add Preseason stage
  data_pre <- data %>%
    dplyr::distinct(Crop, Season, Time, Date) %>%
    dplyr::mutate(Stage = "Preseason", CumPercentage = 1) %>%
    dplyr::ungroup()

  data <- dplyr::bind_rows(data_pre, data)

  # Arrange the columns
  data <- data[ , c("Crop", "Stage", "Season", "Time", "Date", "CumPercentage")]

  # Split by crop
  data <- split(data, f = data$Crop)

  # Handle stages (factorize, ignore missing for whole seasons)
  data <- lapply(data, calc_stages)

  # Create Percentage from CumPercentage
  data <- lapply(data, calc_percentage, cum = FALSE)

  # Result
  data

}

#' @rdname tidy_progress
#' @export
calc_stages <- function(data) {

  # Bind global variables
  Date <- Stage <- CumPercentage <- Percentage <- Time <- NULL

  data_stages <- data %>%
    dplyr::group_by(Stage) %>%
    dplyr::summarise(start = weighted.mean(CumPercentage, Time),
                     .groups = "keep") %>%
    dplyr::arrange(desc(start)) %>%
    dplyr::filter(!is.na(start)) %>%
    dplyr::ungroup()

  data$Stage <- factor(data$Stage, levels = data_stages$Stage, ordered = TRUE)
  dplyr::filter(data, !is.na(Stage))

}

#' @rdname tidy_progress
#' @export
calc_percentage <- function(data, cum = TRUE) {

  # Bind global variables
  Date <- Stage <- CumPercentage <- Percentage <- Season <- Time <- NULL

  if (cum) {
    data %>%
      dplyr::arrange(Season, Time, desc(Stage)) %>%
      dplyr::group_by(Season, Time) %>%
      dplyr::mutate(CumPercentage = trunc_minmax(cumsum(Percentage), 0, 1)) %>%
      dplyr::mutate(Percentage = CumPercentage - lag(CumPercentage, default = 0)) %>%
      dplyr::arrange(Season, Time, Stage) %>%
      dplyr::ungroup()
  } else {
    data %>%
      dplyr::arrange(Season, Time, desc(Stage)) %>%
      dplyr::group_by(Season, Time) %>%
      dplyr::mutate(CumPercentage = trunc_minmax(CumPercentage, 0, 1)) %>%
      dplyr::mutate(Percentage = CumPercentage - lag(CumPercentage, default = 0)) %>%
      dplyr::arrange(Season, Time, Stage) %>%
      dplyr::ungroup()
  }

}
