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
    
    ###sample_info
    if(x@activated == "sample_info"){
      if(!"sample_id" %in% colnames(new_x)){
        stop("You can't remove sample_id.\n")
      }
    }
    
    ###variable_info
    if(x@activated == "variable_info"){
      if(!"variable_id" %in% colnames(new_x)){
        stop("You can't remove variable_id\n")
      }
    }
    
    x <- update_sample_info(x)
    x <- update_variable_info(x)
    return(x)
  }

#' @importFrom dplyr left_join
#' @export
dplyr::left_join
