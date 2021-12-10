#' @method left_join mass_dataset
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
    
    x =
      slot(object = x, name = x@activated)
    
    x =
      left_join(x,
                y,
                by = by,
                copy = copy,
                suffix = suffix,
                ...)
    
    slot(object = x, name = x@activated) = x
    return(x)
  }

#' @importFrom dplyr left_join
#' @export
dplyr::left_join
