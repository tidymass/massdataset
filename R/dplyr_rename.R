#' @method rename mass_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr rename
#' @export
rename.mass_dataset <-
  function(.data, ...) {
    dots <- rlang::quos(...)
    
    if (length(.data@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    x <-
      slot(object = .data, name = .data@activated)
    
    x <-
      dplyr::rename(x, !!!dots)
    
    slot(object = .data, name = .data@activated) = x
    
    if (.data@activated == "expression_data") {
      .data@sample_info$sample_id = colnames(x)
    }
    
    if (.data@activated == "sample_info") {
      .data@sample_info_note$name <-
        colnames(.data@sample_info)
    }
    
    if (.data@activated == "variable_info") {
      .data@variable_info_note$name <-
        colnames(.data@variable_info)
    }
    
    return(.data)
  }

#' @importFrom dplyr rename
#' @export
dplyr::rename
