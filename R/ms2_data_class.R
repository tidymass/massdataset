##S4 class for ms2_data
#' An S4 class that stores the ms2_data
#' @name ms2_data
#' @docType class
#' @slot column column
#' @slot polarity polarity
#' @slot variable_id variable_id
#' @slot ms2_spectrum_id ms2_spectrum_id
#' @slot ms2_spectra ms2_spectra
#' @slot mz_tol mz_tol
#' @slot rt_tol rt_tol
#' @exportClass ms2_data
setClass(
  Class = "ms2_data",
  representation(
    column = "character",
    polarity = "character",
    variable_id = "character",
    ms2_spectrum_id = "character",
    ms2_spectra = "list",
    mz_tol = "numeric",
    rt_tol = "numeric"
  )
)

#' show method for ms2_data
#' @name show
#' @docType methods
#' @rdname show-methods
#' @title show method
#' @param object A \code{ms2_data} instance.
#' @return message
#' @importFrom methods show
#' @export
#' @author Xiaotao Shen
setMethod(
  f = "show",
  signature(object = "ms2_data"),
  definition = function(object) {
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("column:", object@column), "\n")
    cat(crayon::green("polarity:", object@polarity), "\n")
    cat(crayon::green("mz_tol:", object@mz_tol), "\n")
    cat(crayon::green("rt_tol (second):", object@rt_tol), "\n")
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green(length(unique(object@variable_id)), 
                      "variables:\n"))
    cat(crayon::green(head(unique(object@variable_id), 5)))
    cat(crayon::green("...\n"))
    cat(crayon::green(length(unique(object@ms2_spectrum_id)), "MS2 spectra.\n"))
    cat(crayon::green(head(unique(object@ms2_spectrum_id), 5)))
    cat(crayon::green("...\n"))
  }
)
