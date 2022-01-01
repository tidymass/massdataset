#' @method group_by mass_dataset
#' @importFrom rlang quos !!!
#' @importFrom dplyr group_by
#' @export
group_by.mass_dataset <-
  function(.data, ...) {
    dots <- rlang::quos(...)
    
    if (length(.data@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    x =
      slot(object = .data, name = .data@activated)
    
    x =
      group_by(x, !!!dots)
    
    slot(object = .data, name = .data@activated) = x
    
    if (.data@activated == "sample_info") {
      .data@expression_data = .data@expression_data[, x$sample_id]
    }
    
    if (.data@activated == "variable_info") {
      .data@expression_data = .data@expression_data[x$variable_id, ]
    }
    
    return(.data)
  }

#' @importFrom dplyr group_by
#' @export
dplyr::group_by

#' @importFrom dplyr desc
#' @export
dplyr::desc
