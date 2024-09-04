#' @method pull mass_dataset
#' @docType methods
#' @importFrom rlang quos !!! enquo
#' @importFrom dplyr pull
#' @export
pull.mass_dataset <- function(.data, var = -1, ...) {
  # dots <- quos(...)
  var <- enquo(var)
  if (length(.data@activated) == 0) {
    stop("activate you object using activate_mass_dataset first.\n")
  }
  
  temp_slot =
    slot(object = .data, name = .data@activated)
  
  dplyr::pull(temp_slot, !!var, ...)
}

#' @importFrom dplyr pull
#' @export
dplyr::pull
