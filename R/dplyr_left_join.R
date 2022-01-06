#' @method left_join mass_dataset
#' @docType methods
#' @importFrom dplyr left_join
#' @export
left_join.mass_dataset <-
  function(x,
           y,
           by = NULL,
           copy = FALSE,
           suffix = c(".x", ".y"),
           ...) {
    if (length(x@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    new_x =
      slot(object = x, name = x@activated)
    
    new_x =
      left_join(new_x,
                y,
                by = by,
                copy = copy,
                suffix = suffix,
                ...)
    
    slot(object = x, name = x@activated) = new_x
    return(x)
  }

#' @importFrom dplyr left_join
#' @export
dplyr::left_join
