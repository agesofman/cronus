#-------------------------------------------------------------------------------
# Class definitions
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Region class
#'
#' @description
#' An S4 class that defines a region of interest.
#'
#' @slot name character. The name of the region.
#' @slot type character. The type of the region.
#' @slot id ANY. An appropriate ID for the region.
#' @slot div character. A named character vector specifying the divisions,
#' starting from the country level and stopping at the region's level.
#'
#' @return an object of class `Region`.
#' @export
#'
#' @examples
#' \dontrun{
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#'
#' region <- Region(name = "nance", type = "us county",
#'                  div = c(country = "United States", state = "Nebraska", county = "Nance"))
#' }
Region <- setClass("Region",
                   slots = list(name = "character",
                                type = "character",
                                id = "ANY",
                                div = "character"),
                   prototype = list(id = "",
                                    div = ""))

#' Cronus database classes
#'
#' @description
#' cronus creates a database, which takes the form of a 5-level directory. These
#' levels are Region > Sector > Provider > Product > Variable.
#'
#' @details
#' For a specific region, data can belong to one of three sectors: agricultural,
#' environmental or satellite. Data are made available by a provider, usually an
#' organization or a lab, which organizes the data in one or more products.
#' Each product contains several variables of interest.
#'
#' The class tree structure is the following:
#'
#' - \code{Agricultural} Sector
#'    - \code{Nass} Provider
#'      - \code{Quickstats} Product
#'        - \code{Progress} Variable
#'      - \code{Cropmaps} Product
#'    - \code{Bibliography} Provider
#'      - \code{Parameters} Product
#' - \code{Environmental} Sector
#'    - \code{Ornl} Provider
#'      - \code{Daymet} Product
#' - \code{Satellite} Sector
#'    - \code{Modis} Provider
#'      - \code{Mod09ga} Product
#'      - \code{Myd09ga} Product
#'    - \code{Landsat} Provider
#'      - \code{Landsat_tm_c1} Product
#'      - \code{Landsat_etm_c1} Product
#'      - \code{Landsat_8_c1} Product
#'    - \code{Sentinel} Provider
#'      - \code{S2msi2a} Product
#'      - \code{Sy_2_syn___} Product
#'
#' Classes \code{Database}, \code{Sector}, \code{Provider}, and \code{Product}
#' are also defined, but are abstract classes that exist solely for hierarchical
#' purposes.
#'
#' All classes are defined with the S4 OOP system, except from the Variables
#' that are defined using the S3 OOP system. Each variable has its own
#' documentation page.
#'
#' @slot region Region. A region of interest.
#' @slot date Date. Dates of interest (vector).
#' @slot dir character. The database parent directory. If the path_demeter
#' environment variable has been set, it can be omitted.
#'
#' @return An S4 object of the appropriate class (constructor functions).
#' @export
#'
#' @seealso [cronus::set_path_demeter()]
#'
#' @examples
#' \dontrun{
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#' date <- as.Date("2020-07-15")
#' dir <- getwd()
#'
#' w <- new("Quickstats", region = region, date = date, dir = dir)
#' x <- new("Cropmaps", region = region, date = date, dir = dir)
#' y <- new("Daymet", region = region, date = date, dir = dir)
#' z <- new("Mod09ga", region = region, date = date, dir = dir)
#'
#' }
Database <- setClass("Database",
                     slots = list(region = "Region",
                                  date = "Date",
                                  dir = "character"),
                     prototype = list(date = as.Date(""),
                                      dir = get_path_demeter()))

#' @rdname Database-class
Sector <- setClass("Sector", contains = "Database")

#' @rdname Database-class
Provider <- setClass("Provider", contains = "Sector")

#' @rdname Database-class
Product <- setClass("Product", contains = "Provider")

#' @rdname Database-class
Variable <- setClass("Variable", contains = "Product")

# Sectors ----

#' @rdname Database-class
Agricultural <- setClass("Agricultural", contains = c("Sector"))

#' @rdname Database-class
Environmental <- setClass("Environmental", contains = c("Sector"))

#' @rdname Database-class
Satellite <- setClass("Satellite", contains = c("Sector"))

# Providers ----

#' @rdname Database-class
Nass <- setClass("Nass", contains = c("Provider", "Agricultural"))

#' @rdname Database-class
Bibliography <- setClass("Bibliography", contains = c("Provider", "Agricultural"))

#' @rdname Database-class
Ornl <- setClass("Ornl", contains = c("Provider", "Environmental"))

#' @rdname Database-class
Modis <- setClass("Modis", contains = c("Provider", "Satellite"))

#' @rdname Database-class
Landsat <- setClass("Landsat", contains = c("Provider", "Satellite"))

#' @rdname Database-class
Sentinel <- setClass("Sentinel", contains = c("Provider", "Satellite"))

# Products ----

#' @rdname Database-class
Quickstats <- setClass("Quickstats", contains = c("Product", "Nass"))

#' @rdname Database-class
Cropmaps <- setClass("Cropmaps", contains = c("Product", "Nass"))

#' @rdname Database-class
Parameters <- setClass("Parameters", contains = c("Product", "Bibliography"))

#' @rdname Database-class
Daymet <- setClass("Daymet", contains = c("Product", "Ornl"))

#' @rdname Database-class
Mod09ga <- setClass("Mod09ga", contains = c("Product", "Modis"))

#' @rdname Database-class
Myd09ga <- setClass("Myd09ga", contains = c("Product", "Modis"))

#' @rdname Database-class
Landsat_tm_c1 <- setClass("Landsat_tm_c1", contains = c("Product", "Landsat"))

#' @rdname Database-class
Landsat_etm_c1 <- setClass("Landsat_etm_c1", contains = c("Product", "Landsat"))

#' @rdname Database-class
Landsat_8_c1 <- setClass("Landsat_8_c1", contains = c("Product", "Landsat"))

#' @rdname Database-class
S2msi2a <- setClass("S2msi2a", contains = c("Product", "Sentinel"))

#' @rdname Database-class
Sy_2_syn___ <- setClass("Sy_2_syn___", contains = c("Product", "Sentinel"))
