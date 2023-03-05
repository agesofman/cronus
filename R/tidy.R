#-------------------------------------------------------------------------------
# Tidy raw data
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Tidy Quickstats progress
#'
#' @description
#' Tidy the Quickstats progress data.
#'
#' @param data data.frame. The quickstats progress variable.
#'
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @return A `ProgressList`.
#'
#' @details
#' This function relies on the `tidyverse` package to tidy the data.
tidy_Qs_progress <- function(data) {

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

  for (i in seq_along(data)) {

    # Handle stages (factorize, ignore missing for whole seasons)
    data_stages <- data[[i]] %>%
      dplyr::group_by(Stage) %>%
      dplyr::summarise(start = weighted.mean(CumPercentage, Time), .groups = "keep") %>%
      dplyr::arrange(desc(start)) %>%
      dplyr::filter(!is.na(start)) %>%
      dplyr::ungroup()

    data[[i]] <- data[[i]] %>%
      dplyr::mutate(Stage = factor(Stage, levels = data_stages$Stage, ordered = TRUE)) %>%
      dplyr::filter(!is.na(Stage)) %>%
      # Create Percentage from CumPercentage
      dplyr::arrange(Season, Time, desc(Stage)) %>%
      dplyr::group_by(Season, Time) %>%
      dplyr::mutate(CumPercentage = trunc_minmax(CumPercentage, 0, 1)) %>%
      dplyr::mutate(Percentage = CumPercentage - lag(CumPercentage, default = 0)) %>%
      dplyr::arrange(Season, Time, Stage) %>%
      dplyr::ungroup()

  }

  # Result
  ProgressList(data)

}

#' @rdname tidy_Qs_progress
#' @export
calc_cumperc <- function(data) {
  UseMethod("calc_cumperc")
}

#' @rdname tidy_Qs_progress
#' @export
calc_cumperc.Progress <- function(data) {

  # Bind global variables
  Date <- Stage <- CumPercentage <- Percentage <- Season <- Time <- NULL

  data <- data %>%
    dplyr::arrange(Season, Time, desc(Stage)) %>%
    dplyr::group_by(Season, Time) %>%
    dplyr::mutate(CumPercentage = trunc_minmax(cumsum(Percentage), 0, 1)) %>%
    dplyr::mutate(Percentage = CumPercentage - lag(CumPercentage, default = 0)) %>%
    dplyr::arrange(Season, Time, Stage) %>%
    dplyr::ungroup()

  Progress(data)

}

#' @rdname tidy_Qs_progress
#' @export
calc_cumperc.ProgressList <- function(data) {

  data <- lapply(data, calc_cumperc)
  ProgressList(data)

}

#' @rdname tidy_Qs_progress
#' @export
calc_perc <- function(data) {
  UseMethod("calc_perc")
}

#' @rdname tidy_Qs_progress
#' @export
calc_perc.Progress <- function(data) {

  # Bind global variables
  Date <- Stage <- CumPercentage <- Percentage <- Season <- Time <- NULL

  data <- data %>%
    dplyr::arrange(Season, Time, desc(Stage)) %>%
    dplyr::group_by(Season, Time) %>%
    dplyr::mutate(CumPercentage = trunc_minmax(CumPercentage, 0, 1)) %>%
    dplyr::mutate(Percentage = CumPercentage - lag(CumPercentage, default = 0)) %>%
    dplyr::arrange(Season, Time, Stage) %>%
    dplyr::ungroup()

  Progress(data)

}

#' @rdname tidy_Qs_progress
#' @export
calc_perc.ProgressList <- function(data) {

  data <- lapply(data, calc_perc)
  ProgressList(data)

}
