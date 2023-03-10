% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hermes.R
\name{get_path_hermes_obj}
\alias{get_path_hermes_obj}
\alias{save_obj}
\alias{read_obj}
\title{Save and read objects}
\usage{
get_path_hermes_obj(name, project, dir = get_path_hermes())

save_obj(object, name, project, dir = get_path_hermes())

read_obj(name, project, dir = get_path_hermes())
}
\arguments{
\item{name}{character. The name of the script (without the .RData suffix).}

\item{project}{character. The project the script concerns.}

\item{dir}{character. The hermes project directory.}

\item{object}{ANY. The object to save / load.}
}
\description{
This set of functions saves and reads files inside the \code{hermes}
structured directory.
}
\examples{
\dontrun{
get_path_hermes_obj("objectname", "projectname")

x <- 1:3
save_obj(x, "objectname", "projectname")
read_obj("objectname", "projectname")
}
}
