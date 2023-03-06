#-------------------------------------------------------------------------------
# Modify data
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title modify data
#'
#' @param data data.frame. The data to modify.
#' @param vars character. The name of the variables (columns) of `data`.
#' @param fun function. The name of a function to apply on `data`.
#' @param ... extra arguments.
#'
#' @return An object of the same class as `data`.
#'
#' @importFrom dplyr group_by mutate across ungroup
#' @export
setGeneric("modify", signature = c("data"),
           function(data, ...) { standardGeneric("modify") })

#' @rdname modify
setMethod("modify",
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

#' @rdname modify
#' @export
setMethod("modify",
          signature  = c(data = "ProgressList"),
          definition = function(data, vars, fun = cumsum) {

  data <- lapply(data, modify, vars = vars, fun = fun)

  # Return the result
  ProgressList(data)

})
