#' @title [ method
#' @description Subsets a `mass_dataset` object by variables (`i`) and samples
#'   (`j`) using the `[` operator. The method updates the corresponding slots
#'   in the returned object to keep the data internally consistent.
#' @author Xiaotao Shen
#' \email{xiaotao.shen@outlook.com}
#' @method [ mass_dataset
#' @param x A `mass_dataset` object.
#' @param i Row indices or variable identifiers to keep.
#' @param j Column indices or sample identifiers to keep.
#' @param drop Logical; if `TRUE`, allow base subsetting to simplify the
#'   extracted `expression_data`.
#' @param ... Additional arguments passed to base subsetting.
#' @export
#' @return A subsetted `mass_dataset` object, or a simplified extracted object
#'   if base subsetting returns a non-data-frame result.
`[.mass_dataset` <-
  function(x, i, j, drop = FALSE, ...) {
    if (missing(i) & missing(j)) {
      return(x)
    }
    # browser()
    ####don't provide i
    if (!missing(i)) {
      if (is.character(i)) {
        i <- match(i, rownames(x@expression_data))
      }
    } else{
      i = seq_len(nrow(x@expression_data))
    }
    
    ####don't provide j
    if (!missing(j)) {
      if (is.character(j)) {
        j <- match(j, colnames(x@expression_data))
      }
    } else{
      j = seq_len(ncol(x@expression_data))
    }
    
    ####NA in j
    if (sum(is.na(j)) > 0) {
      j = j[!is.na(j)]
      if (length(j) == 0) {
        j = seq_len(ncol(x))
        warning("All sample index (j) are not in the object. Please check.")
      } else{
        warning("Some sample index (j) are not in the object. Please check.")
      }
    }
    
    ####if j is negative and some j is not in range
    if (j[1] < 0 & any(!abs(j) %in% seq_len(ncol(x)))) {
      warning("Some sample index (j) are not in the object. Please check.")
      j = j[j %in% -seq_len(ncol(x))]
    }
    
    ####if j is positive and some j is not in range
    if (j[1] > 0 & any(!j %in% seq_len(ncol(x)))) {
      warning("Some sample index (j) are not in the object. Please check.")
      j = j[j %in% seq_len(ncol(x))]
    }
    
    ###NA in j
    if (sum(is.na(i)) > 0) {
      i = i[!is.na(i)]
      if (length(i) == 0) {
        i = seq_len(nrow(x))
        warning("Some variable index (i) are not in the object. Please check.")
      } else{
        warning("Some variable index (i) are not in the object. Please check.")
      }
      
    }
    
    ####if i is negative and some i is not in range
    if (i[1] < 0 & any(!abs(i) %in% seq_len(nrow(x)))) {
      warning("Some variable index (i) are not in the object. Please check.")
      i = i[i %in% -seq_len(nrow(x))]
    }
    
    ####if i is positive and some i is not in range
    if (i[1] > 0 & any(!i %in% seq_len(nrow(x)))) {
      warning("Some variable index (i) are not in the object. Please check.")
      i = i[i %in% seq_len(nrow(x))]
    }
    
    ###add paramters
    ####add parameters
    process_info <-
      extract_process_info(x)
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "[",
      parameter = list("i" = i,
                       "j" = j),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "subset")) {
      process_info$subset <-
        parameter
    } else{
      process_info$subset <-
        c(process_info$subset, parameter)
    }
    
    x@process_info <-
      process_info
    
    expression_data <-
      x@expression_data[i, j, drop = drop]
    
    if (!is.data.frame(expression_data)) {
      # expression_data = as.numeric(expression_data)
      return(expression_data)
    }
    
    x@expression_data <-
      x@expression_data[i, j, drop = drop]
    x@sample_info <-
      x@sample_info[j, , drop = FALSE]
    x@variable_info <-
      x@variable_info[i, , drop = FALSE]
    
    return(x)
  }



#' @title $ method
#' @description Extracts a column from the `expression_data` slot of a
#'   `mass_dataset` object by name.
#' @method $ mass_dataset
#' @param x A `mass_dataset` object.
#' @param name A column name from `expression_data`.
#' @return The requested column from `expression_data`, if it exists.
#' @export
`$.mass_dataset` <- function(x, name) {
  out <- .subset2(x@expression_data, name)
  if (is.null(out)) {
    warn(paste0("Unknown or uninitialised column: ", tick(name), "."))
  }
  out
}

tick <- function(x) {
  ifelse(is.na(x), "NA", encodeString(x, quote = "`"))
}
