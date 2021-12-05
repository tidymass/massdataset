#' @method rename mass_dataset
#' @importFrom rlang quos !!!
#' @importFrom dplyr rename
#' @export
rename.mass_dataset <- function(.data, ...) {
  dots <- quos(...)
  
  if (length(.data@activated) == 0) {
    stop("activate you object using activate_mass_dataset first.\n")
  }
  
  x =
    slot(object = .data, name = .data@activated)
  
  x =
    rename(x, !!!dots)
  
  slot(object = .data, name = .data@activated) = x
  
  if (.data@activated == "expression_data") {
    .data@sample_info$sample_id = colnames(x)
  }
  
  return(.data)
}

#' @importFrom dplyr rename
#' @export
dplyr::rename
