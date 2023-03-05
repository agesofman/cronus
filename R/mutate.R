#-------------------------------------------------------------------------------
# Mutate data
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Mutate data
#'
#' @param data data.frame. The data to mutate.
#' @param vars character. The name of the variables (columns) of `data`.
#' @param fun function. The name of a function to apply on `data`.
#' @param ... extra arguments.
#'
#' @return An object of the same class as `data`.
#'
#' @importFrom dplyr group_by mutate across ungroup
#' @export
setGeneric("mutate", signature = c("data"),
           function(data, ...) { standardGeneric("mutate") })

#' @rdname mutate
setMethod("mutate",
          signature  = c(data = "Progress"),
          definition = function(data, vars, fun = cumsum) {

  # Bind global variables
  Stage <- Season <- NULL

  data <- data %>%
    dplyr::group_by(Season, Stage) %>%
    dplyr::mutate(dplyr::across(.cols = vars, .fns = fun, .names = "a{.col}")) %>%
    dplyr::ungroup()

  # Return the result
  Progress(data)

})

#' @rdname mutate
#' @export
setMethod("mutate",
          signature  = c(data = "ProgressList"),
          definition = function(data, vars, fun = cumsum) {

  data <- lapply(data, mutate, vars = vars, fun = fun)

  # Return the result
  ProgressList(data)

})
