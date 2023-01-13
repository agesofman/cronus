#-------------------------------------------------------------------------------
# Plot data
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Plot
#'
#' @description
#' Create a plot of the data stored in the database.
#'
#' @param x an S4 object. A product of interest.
#' @param y character. A variable of interest.
#' @param year numeric. Year(s) of interest.
#' @param date character Date(s) of interest.
#' @param crops character. The crops of interest.
#' @param ncol numeric. Number of rows in the plot layout.
#' @param nrow numeric. Number of columns in the plot layout.
#' @param ylab character. The y-axis title.
#' @param xlab character. The x-axis title.
#' @param save logical. Should the plot be saved?
#' @param dir character. The directory in which the plot will be saved.
#' @param file character. The file name.
#' @param width numeric. The width of the plot in inches.
#' @param height numeric. The height of the plot in inches.
#' @param ... extra arguments.
#'
#' @return A plot, created either with `ggplot2` or with `terra`.
#'
#' @export
#' @importFrom ggplot2 ggplot geom_line aes labs theme_minimal
#' @importFrom dplyr filter
#' @importFrom terra rast cats set.cats plot
#' @importFrom ggpubr ggarrange
#' @importFrom stringr str_to_title
#'
#' @examples
#' \dontrun{
#' # Define required variables
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- date_seq("2002-01-01", "2002-12-31")
#'
#' ## Quickstats Progress
#'
#' # Create the object
#' x <- new("Cropmaps", region = region, date = date)
#'
#' # Plot
#' variable <- "cdl"
#' plot(x, variable, crops = c("Soybeans", "Winter Wheat"), year = 2002)
#'
#' ## Daymet Tmin
#'
#' # Create the object
#' x <- new("Daymet", region = region, date = date, dir = dir)
#'
#' # Plot
#' plot(x, "tmin", "2002-01-01")
#' }
setGeneric("plot")

#' @rdname plot
setMethod("plot",
          signature  = c(x = "Quickstats", y = "character"),
          definition = function(x, y,
                               year,
                               crops = NULL,
                               ncol = NULL,
                               nrow = NULL,
                               ylab = "Percentage",
                               xlab = "Time",
                               save = FALSE,
                               dir = getwd(),
                               file = "plot.pdf",
                               width = 15,
                               height = 8) {

  # Save the plot
  if (save) pdf(file.path(dir, file), width = width, height = height)

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)
  variable <- y
  data <- read(x, variable)

  # Get rtoi
  if (is.null(year)) {
    toi <- get_toi(date)
    year <- toi$uyear
  }

  # Get the crops
  if (is.null(crops)) {
    crops <- names(data)
  }

  # Create the plots
  plots <- list()
  for (crop in crops) {
    data[[crop]] <- dplyr::filter(data[[crop]], .data$Season %in% year)
    plots[[crop]] <- ggplot2::ggplot() +
                     ggplot2::geom_line(data = data[[crop]],
                                        ggplot2::aes(x = .data$Date,
                                                     y = .data$CumPercentage,
                                                     col = .data$Stage)) +
                     ggplot2::labs(title = paste0(crop, " ", variable),
                                   y = ylab,
                                   x = xlab) +
                     ggplot2::theme_minimal()
  }
  print(do.call(ggpubr::ggarrange, c(plots, ncol = ncol, nrow = nrow)))

  # Close the device
  if (save) dev.off()

})

#' @rdname plot
setMethod("plot",
          signature  = c(x = "Cropmaps", y = "character"),
          definition = function(x, y, crops = NULL, year = NULL) {

  # Get slots
  region <- x@region
  date <- x@date
  dir <- x@dir
  product <- get_product(x)
  variable <- y

  # Get toi
  toi <- get_toi(date)
  if (is.null(year)) {
    year <- toi$uyear
  }

  # Get the directories
  dir_var <- create_db(dir, region, product = product, variable = variable)
  path_var <- file.path(dir_var, paste0(year, ".tif"))
  cdl <- terra::rast(path_var)

  if (!is.null(crops)) {
    categ <- terra::cats(cdl)[[1]]
    categ <- categ[categ$CLASS_NAME %in% crops, ]
    terra::set.cats(cdl, value = categ)
  }

  # Create the plot
  main <- paste0("Cropland Data Layer - ", stringr::str_to_title(region@name), " ", year)
  suppressWarnings(terra::plot(cdl, main = main, axes = FALSE, type = "classes"))

})

#' @rdname plot
setMethod("plot",
          signature  = c(x = "Daymet", y = "character"),
          definition = function(x, y, date = NULL) {

  # Get slots
  region <- x@region
  dir <- x@dir
  product <- get_product(x)
  variable <- y

  # Get toi
  if (is.null(date)) {
    date <- x@date
  }
  toi <- get_toi(date)

  # Get the directories
  dir_var <- create_db(dir, region, product = product, variable = variable)
  path_var <- file.path(dir_var, paste0(toi$name, ".tif"))
  ras <- terra::rast(path_var)

  main <- paste0("Daymet ", y, " - ", stringr::str_to_title(region@name), " ", toi$date)
  terra::plot(ras, main = main, axes = FALSE)

})
