% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{download}
\alias{download}
\alias{download,Quickstats-method}
\alias{download,Cropmaps-method}
\alias{download,Daymet-method}
\alias{download,Satellite-method}
\title{Download data}
\usage{
download(x, ...)

\S4method{download}{Quickstats}(x, variable, ringname = NULL)

\S4method{download}{Cropmaps}(x, variable = NULL)

\S4method{download}{Daymet}(x, variable = NULL)

\S4method{download}{Satellite}(x, ringname = NULL)
}
\arguments{
\item{x}{S4 object. The product of interest.}

\item{...}{extra arguments.}

\item{variable}{character. The variable of interest.}

\item{ringname}{character. A keyring to handle the provider credentials.}
}
\value{
For most products, nothing is returned. The data are saved directly
in the cronus database. For lightweight data (such as variables of the
Quickstats product), the downloaded data are returned.
}
\description{
Download agricultural, environmental, or satellite data for a region and time
of interest.
}
\details{
Currently, the function can download:
\itemize{
\item The Quickstats product of NASS.
\item The Cropland Data Layer variable of NASS.
\item The Daymet product of ORNL.
\item The MOD09GA and MYD09GA products of MODIS.
\item The Landsat TM, ETM and 8 products.
\item The Sentinel-2 MSI and Sentinel-3 Synergy products.
}

Details concerning the Quickstats product:
The function is a wrapper of the \code{nassqs()} function from the \code{rnassqs}
package. In order to download the data, an API key is required; information
can be found in the \code{rnassqs} package vignette.

Details concerning the Daymet product:
The S4 class \code{Daymet} can be used to handle data of the daymet product. It is
a wrapper of the \code{get_daymet()} function from the \code{FedData} package.

Details concerning the satellite sector:
This function uses the \code{rsat::rsat_search()} function to search for available
products in the requested dates. Then, it recursively calls
\code{rsat::rsat_download()} and \code{rsat::rsat_mosaic()} to download and mosaic the
data, before deleting the original hdf files to save disk space. The
satellite data are extracted in GeoTiff format (zipped).
The recursive call of the function has a slight performance impact, but it is
crucial in order to download data over large regions and periods of time.
For example, assume we want to download the MOD09GA product for the state of
Nebraska for one day. The (3) hdf files required combined surpass 250MB of
memory space, while the mosaiced, zipped Geotiff images take about 15MB.
}
\examples{
\dontrun{
# Define required variables
region <- Region(name = "nebraska", type = "us state",
                 div = c(country = "United States", state = "Nebraska"))
date <- date_seq("2002-01-01", "2002-12-31")

## Quickstats Progress

# Create the object
x <- new("Quickstats", region = region, date = date)

# Download the data
data <- download(x, "progress", ringname)
class(data)
class(data[["Corn"]])
head(data$Corn)

# Plot the data
plot(data, crops = "Winter Wheat", year = 2021)

## Cropmaps CDL

# Create the object
x <- new("Cropmaps", region = region, date = date)

# Download the data
download(x, "cdl")

## Daymet

# Create the object
x <- new("Daymet", region = region, date = date)

# Download the data
download(x, c("tmin", "tmax"))

## MOD09GA

# Create the object
x <- new("Mod09ga", region = region, date = date)

# Download the data
download(x, ringname)
}
}
