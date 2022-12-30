#-------------------------------------------------------------------------------
# Plot data
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Plot
#'
#' @description `r lifecycle::badge("stable")`
#'
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
#' @return A plot, created either with \code{ggplot2} or with \code{terra}.
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
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- date_seq("2002-01-01", "2002-12-31")
#' dir <- getwd()
#'
#' # Plot the Cropland Data Layer
#' x <- new("Cropmaps", region = region, date = date, dir = dir)
#' variable <- "cdl"
#' download(x, variable)
#' plot(x, variable, crops = c("Soybeans", "Winter Wheat"), year = 2002)
#'
#' # Plot the Daymet minimum temperature
#' x <- new("Daymet", region = region, date = date, dir = dir)
#' download(x, variable = "tmin")
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

  main <- paste0("Cropland Data Layer - ", stringr::str_to_title(region@name), " ", year)
  terra::plot(cdl, main = main, axes = FALSE)
})

#' @rdname plot
setMethod("plot",
          signature  = c(x = "Daymet", y = "character"),
          definition = function(x, y, date) {

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

# plot_mean <- function(x, crops, date) {
#   # Get the toi
#   toi <- get_toi(date)
#   year <- get_toi(date)$uyear
#
#   y <- do.call("rbind", x)
#   y$Date <- pdoy_to_date(y$pdoy)
#   names(y)[1] <- "Crop"
#   y$Year = lubridate::year(y$Date)
#   z <- dplyr::filter(y, Crop %in% crops)
#   z <- dplyr::filter(z, Year %in% year)
#   ggplot(z) +
#     geom_line(aes(x = Date, y = ndvi_smooth_wt1, col = Crop)) +
#     xlim(date[1], date[length(date)]) +
#     ylim(0, 1) +
#     theme_minimal()
# }
#
# plot_pixels <- function(crop = NULL, size = 20, region, date, dir = getwd(), seed = 32091) {
#
#   # Get the toi
#   toi <- get_toi(date)
#
#   # Get the directories
#   product1 <- "MOD09GA"
#   variable1 <- "ndvi_smooth_wt1"
#   product2 <- "cropmaps"
#   variable2 <- "cdl1"
#   dir_var1 <- create_db(dir, region, product = product1, variable = variable1)
#   dir_var2 <- create_db(dir, region, product = product2, variable = variable2)
#   dir_var3 <- create_db(dir, region, product = product1, variable = "tseries")
#   path_var1 <- file.path(dir_var1, paste0(toi$name, ".tif"))
#   path_var2 <- file.path(dir_var2, paste0(toi$uyear, ".tif"))
#   path_var3 <- file.path(dir_var3, paste0(variable1, ".RData"))
#
#   # Load the rasters
#   year <- get_toi(date)$uyear
#   ras_var1 <- terra::rast(path_var1[toi$year == year])
#   ras_var2 <- terra::rast(path_var2[toi$uyear == year])
#   ras_var2 <- terra::project(ras_var2, ras_var1, method = "near")
#   set.seed(seed)
#   x <- terra::as.matrix(ras_var2 == crop)
#   y <- ras_var1[which(x == TRUE)[sample(1:sum(x, na.rm = TRUE), size = size)]]
#
#   df = data.frame(x = rep(date, each = size),
#                   y = as.numeric(t(do.call("rbind", y))),
#                   pixel = rep(1:size, length(date)))
#
#   ggplot(df) +
#     geom_line(aes(x = x, y = y, group = pixel)) +
#     xlim(date[1], date[length(date)]) +
#     ylim(0, 1) +
#     labs(title = paste0(crop, " pixels, ", year), ylab = "NDVI", xlab = "Date") +
#     theme_minimal()
#
# }

