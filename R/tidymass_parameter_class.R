#' tidymass_parameter S4 Class
#'
#' An S4 class to represent the parameters used in tidymass functions.
#' @docType class
#' @slot pacakge_name A character vector representing the name of the package where the function resides.
#' @slot function_name A character vector representing the name of the function that uses these parameters.
#' @slot parameter A list containing the parameters used in the function.
#' @slot time A POSIXct object representing the time when the function was called.
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'
#' @exportClass tidymass_parameter

setClass(
  Class = "tidymass_parameter",
  representation(
    pacakge_name = "character",
    function_name = "character",
    parameter = "list",
    time = "POSIXct"
  )
)

setMethod(
  f = "show",
  signature(object = "tidymass_parameter"),
  definition = function(object) {
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("pacakge_name:", object@pacakge_name), "\n")
    cat(crayon::green("function_name:", object@function_name),
        "\n")
    cat(crayon::green("time:", object@time), "\n")
    cat(crayon::green("parameters:\n"))
    for (idx in seq_along(object@parameter)) {
      cat(crayon::green(names(object@parameter)[idx], ":" , object@parameter[idx]), "\n")
    }
  }
)
