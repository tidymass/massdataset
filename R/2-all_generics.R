#' @title Generic Function for Retrieving Sample Information
#'
#' @description 
#' The `sample_info` generic function retrieves sample-related information from a provided object.
#'
#' @param object An object from which sample information will be extracted.
#' @param ... Additional arguments.
#'
#' @return 
#' Depends on the specific method implementation.
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'
#'
#' @rdname sample_info
#' @export 
setGeneric(
  name = "sample_info",
  def = function(object, ...)
    standardGeneric("sample_info")
)


#' @title Generic Function for Retrieving Expression Data
#'
#' @description 
#' The `expression_data` generic function retrieves gene expression-related data from the provided object.
#'
#' @param object An object from which expression data will be extracted.
#' @param ... Additional arguments.
#'
#' @return 
#' Depends on the specific method implementation. Typically, it might return a matrix or dataframe of expression values.
#'
#' @seealso 
#' \code{\link{methods}} for more details on S4 methods.
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'
#' @rdname expression_data
#' @export 
setGeneric(
  name = "expression_data",
  def = function(object, ...)
    standardGeneric(f = "expression_data")
)


#' @title Generic Function for Retrieving Variable Information
#'
#' @description 
#' The `variable_info` generic function retrieves detailed information related to variables in the provided object.
#'
#' @param object An object from which variable information will be extracted.
#' @param ... Additional arguments.
#'
#' @return 
#' Depends on the specific method implementation. Typically, it might return a dataframe or a list describing the variables.
#'
#' @seealso 
#' \code{\link{methods}} for more details on S4 methods.
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'
#' @rdname variable_info
#' @export 
setGeneric(
  name = "variable_info",
  def = function(object, ...)
    standardGeneric(f = "variable_info")
)

#' @title Generic Function for Retrieving Process Information
#'
#' @description 
#' The `process_info` generic function retrieves detailed information related to the data processing of the provided object.
#'
#' @param object An object from which process information will be extracted.
#' @param ... Additional arguments.
#'
#' @return 
#' Depends on the specific method implementation. Typically, it might return a dataframe or a list describing the processing details.
#'
#' @seealso 
#' \code{\link{methods}} for more details on S4 methods.
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#' @rdname process_info
#' @export 
setGeneric(
  name = "process_info",
  def = function(object, ...)
    standardGeneric(f = "process_info")
)

#' @title Generic Function for Retrieving MS2 Spectra Data
#'
#' @description 
#' The `ms2_data` generic function retrieves mass spectrometry (MS2) spectra data from the provided object.
#'
#' @param object An object from which MS2 spectra data will be extracted.
#' @param ... Additional arguments.
#'
#' @return 
#' Depends on the specific method implementation. Typically, it might return a matrix, dataframe, or list with the MS2 spectra information.
#'
#' @seealso 
#' \code{\link{methods}} for more details on S4 methods.
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'
#' @rdname ms2_data
#' @export 
setGeneric(
  name = "ms2_data",
  def = function(object, ...)
    standardGeneric(f = "ms2_data")
)


#' @title Generic Function for Retrieving Sample Information Notes
#'
#' @description 
#' The `sample_info_note` generic function retrieves notes or metadata associated with sample information from the provided object.
#'
#' @param object An object from which sample information notes will be extracted.
#' @param ... Additional arguments.
#'
#' @return 
#' Depends on the specific method implementation. Typically, it might return a character vector, list, or dataframe containing notes or metadata about the sample information.
#'
#' @seealso 
#' \code{\link{methods}} for more details on S4 methods.
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'
#' @rdname sample_info_note
#' @export 

setGeneric(
  name = "sample_info_note",
  def = function(object, ...)
    standardGeneric(f = "sample_info_note")
)

#' @title Generic Function for Retrieving Variable Information Notes
#'
#' @description 
#' The `variable_info_note` generic function retrieves notes or metadata associated with variable information from the provided object.
#'
#' @param object An object from which variable information notes will be extracted.
#' @param ... Additional arguments.
#'
#' @return 
#' Depends on the specific method implementation. Typically, it might return a character vector, list, or dataframe containing notes or metadata about the variable information.
#'
#' @seealso 
#' \code{\link{methods}} for more details on S4 methods.
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'
#' @rdname variable_info_note
#' @export 

setGeneric(
  name = "variable_info_note",
  def = function(object, ...)
    standardGeneric(f = "variable_info_note")
)

#' @title Generic Function for Retrieving Annotation Table
#'
#' @description 
#' The `annotation_table` generic function retrieves the annotation table from the provided object.
#'
#' @param object An object from which the annotation table will be extracted.
#' @param ... Additional arguments.
#'
#' @return 
#' Depends on the specific method implementation. Typically, it might return a dataframe or matrix containing annotation information for the object.
#'
#' @seealso 
#' \code{\link{methods}} for more details on S4 methods.
#'
#' @author Xiaotao Shen \email{shenxt1990@@outlook.com}
#'
#' @rdname annotation_table
#' @export 
setGeneric(
  name = "annotation_table",
  def = function(object, ...)
    standardGeneric(f = "annotation_table")
)

