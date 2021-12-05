#' @method left_join mass_dataset
#' @importFrom dplyr left_join
#' @export
left_join.mass_dataset <-
  function(.data,
           y,
           by = NULL,
           copy = FALSE,
           suffix = c(".x", ".y"),
           ...) {
    if (length(.data@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    x =
      slot(object = .data, name = .data@activated)
    
    x =
      left_join(x,
                y,
                by = by,
                copy = copy,
                suffix = suffix,
                ...)
    
    slot(object = .data, name = .data@activated) = x
    return(.data)
  }

#' @importFrom dplyr left_join
#' @export
dplyr::left_join
