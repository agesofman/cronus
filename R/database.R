#-------------------------------------------------------------------------------
# Manage database structure
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' Provider service
#'
#' @description `r lifecycle::badge("stable")`
#' Get the service that keeps the satellite data stored in order to access the data.
#'
#' @param provider character. A provider of interest
#'
#' @return character. The name of the corresponding service.
#'
#' @examples
#' \dontrun{
#' get_service("modis")
#' get_service("sentinel")
#' }
get_service <- function(provider) {
  if (provider == "modis") {
    return("usgs")
  } else if (provider == "landsat") {
    return(c("usgs", "earthdata"))
  } else if (provider == "sentinel") {
    return("scihub")
  } else {
    stop("Provider must be one of modis, landsat, or sentinel. Cannot find log-in credentials.")
  }
}

#' Manage database
#'
#' @description `r lifecycle::badge("stable")`
#' Helper functions concerning the database structure.
#'
#' @param name character. A class name of interest.
#' @param x S4 object. An object of a class defined in \code{cronus}.
#'
#' @return
#'  - \code{get_level} returns a character, the level name of the structured database.
#'  - \code{is_x} returns a logical. It answers the question "Is name a x?"
#'  - \code{get_x} returns a character, the name of the object's x.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_level("modis")
#' get_level("cdl")
#'
#' is_sector("Agricultural")
#' is_provider("cdl")
#' }
get_level <- function(name) {
  name <- tolower(name)
  col <- which(db_tree == name, arr.ind = TRUE)[ , 2]
  unique(names(db_tree)[col])
}

#' @rdname get_level
is_sector <- function(name) {
  get_level(name) == "Sector"
}

#' @rdname get_level
is_provider <- function(name) {
  get_level(name) == "Provider"
}

#' @rdname get_level
is_product <- function(name) {
  get_level(name) == "Product"
}

#' @rdname get_level
get_class <- function(x) {
  as.character(class(x))
}

#' @rdname get_level
get_sector <- function(x) {
  name <- tolower(get_class(x))
  if (is_product(name)) {
    get_branch(product = name)$sector
  } else if (is_provider(name)) {
    get_branch(provider = name)$sector
  } else if (is_sector(name)) {
    get_branch(sector = name)$sector
  } else {
    stop(name, " is not a Product, Provider, nor Sector. Did you misspell ", name, "?")
  }
}

#' @rdname get_level
get_provider <- function(x) {
  name <- tolower(get_class(x))
  if (is_product(name)) {
    get_branch(product = name)$provider
  } else if (is_provider(name)) {
    get_branch(provider = name)$provider
  } else if (is_sector(name)) {
    stop(name, " is a Sector, a superclass of Provider. It does not have a single Provider.")
  } else {
    stop(name, " is not a Product, Provider, nor Sector. Did you misspell ", name, "?")
  }
}

#' @rdname get_level
get_product <- function(x) {
  name <- tolower(get_class(x))
  if (is_product(name)) {
    get_branch(product = name)$product
  } else if (is_provider(name)) {
    stop(name, " is a Provider, a superclass of Product. It does not have a single Product.")
  } else if (is_sector(name)) {
    stop(name, " is a Sector, a superclass of Product. It does not have a single Product.")
  } else {
    stop(name, " is not a Product, Provider, nor Sector. Did you misspell ", name, "?")
  }
}

#' @title Manage database
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Get the relative or absolute path to a branch of the `cronus` database.
#'
#' @param dir character. The database parent directory.
#' @param region Region The region of interest.
#' @param sector character. The sector of interest.
#' @param provider character. The provider of interest.
#' @param product character. The product of interest.
#' @param variable character. The variable of interest.
#'
#' @return
#' - `get_branch()` returns a list containing the `path` (absolute or relative,
#' depending on whether the parent directory was provided or not), as well as
#' all the arguments provided and those that could be extrapolated from them.
#' - `create_db()` creates the directory specified, `get_branch()$path`, and
#' returns the path invisibly.
#' - `print_tree()` prints the database structure in a Sector - Provider - Product
#' tree form.
#'
#' @importFrom dplyr filter
#' @export
#'
#' @examples
#' \dontrun{
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#'
#' list_cropmaps <- get_branch(dir = getwd(), region = region, product = "cropmaps")
#' list_cropmaps$dir
#' list_cropmaps$region
#' list_cropmaps$sector
#' list_cropmaps$provider
#' list_cropmaps$product
#' list_cropmaps$variable
#' list_cropmaps$path
#'
#' path_cropmaps <- create_db(dir = getwd(), region = region, product = "cropmaps")
#' }
get_branch <- function(dir = NULL,
                       region = NULL,
                       sector = NULL,
                       provider = NULL,
                       product = NULL,
                       variable = NULL) {

  if (is.null(product)) {
    if (is.null(provider)) {
      if (is.null(sector)) {
        path <- ""
      } else {
        x <- dplyr::filter(db_tree, Sector %in% sector)
        path <- x$Sector
      }
    } else {
      x <- dplyr::filter(db_tree, Provider %in% provider)
      path <- file.path(x$Sector, x$Provider)
    }
  } else {
    x <- dplyr::filter(db_tree, Product %in% product)
    path <- file.path(x$Sector, x$Provider, x$Product)
  }

  if (!is.null(variable)) {
    path <- file.path(path, variable)
  }

  if (!is.null(region)) {
    path <- file.path(region, path)
  }

  if (!is.null(dir)) {
    path <- file.path(dir, path)
  }

  list(dir = dir,
       region = region,
       sector = x$Sector,
       provider = x$Provider,
       product = x$Product,
       variable = variable,
       path = path)
}

#' @rdname get_branch
create_db <- function(dir = NULL,
                      region = NULL,
                      sector = NULL,
                      provider = NULL,
                      product = NULL,
                      variable = NULL)  {
  path <- get_branch(dir, region@name, sector, provider, product, variable)$path
  lapply(X = path, FUN = dir.create, showWarnings = FALSE, recursive = TRUE)
  invisible(path)
}

#' @rdname get_branch
#' @export
print_tree <- function() {

  cat("\n")
  cat("Cronus database structure \n")

  for (sector in unique(db_tree$Sector)) {
    db_sec <- dplyr::filter(db_tree, Sector %in% sector)
    cat("\n", sector, "\n")
    for (provider in unique(db_sec$Provider)) {
      db_prov <- dplyr::filter(db_sec, Provider %in% provider)
      cat(rep(" ", 2), "|_", provider, "\n")
      for (product in unique(db_prov$Product)) {
        cat(rep(" ", 4), "|_", product, "\n")
      }
    }
  }
}
