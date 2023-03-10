% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read}
\alias{read}
\alias{read,Quickstats-method}
\alias{read,Cropmaps-method}
\alias{read,Parameters-method}
\alias{read,Daymet-method}
\alias{read,Mod09ga-method}
\title{Read data}
\usage{
read(x, ...)

\S4method{read}{Quickstats}(x, variable, predictors = NULL)

\S4method{read}{Cropmaps}(x, name)

\S4method{read}{Parameters}(x, name)

\S4method{read}{Daymet}(x, variable)

\S4method{read}{Mod09ga}(x, variable)
}
\arguments{
\item{x}{S4 object. A product of interest.}

\item{...}{extra arguments.}

\item{variable}{character. The variable of interest.}

\item{predictors}{data.frame. A data frame with columns Product and Variable.}

\item{name}{character. The name of the metadata files.}
}
\value{
The data of interest (list, data.frame, raster, or other).
}
\description{
Read data stored in the cronus database.
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

# Read the data
variable <- "progress"
list_progress <- read(x, variable)

## Parameters

# Create the object
x <- new("Parameters", region = region, date = date)

# Read the data
read(x, name = "default")
}
}
