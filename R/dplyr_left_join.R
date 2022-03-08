#' @method left_join mass_dataset
#' @importFrom dplyr left_join
#' @export
left_join.mass_dataset <-
  function(x,
           y,
           by = NULL,
           copy = FALSE,
           suffix = c(".x", ".y"),
           ...,
           keep = FALSE) {
    if (length(x@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    if (!is(x, class2 = "mass_dataset")) {
      stop("x must be mass_dataset class.\n")
    }
    
    new_x <-
      slot(object = x, name = x@activated)
    
    new_x <-
      left_join(new_x,
                y,
                by = by,
                copy = copy,
                suffix = suffix,
                ...)
    
    slot(object = x, name = x@activated) <- new_x
    x <- update_sample_info(x)
    x <- update_variable_info(x)
    return(x)
  }

#' @importFrom dplyr left_join
#' @export
dplyr::left_join
