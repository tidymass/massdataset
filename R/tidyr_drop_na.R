#' @method drop_na mass_dataset
#' @importFrom rlang quos !!!
#' @importFrom tidyr drop_na
#' @export
drop_na.mass_dataset <-
  function(data, ...) {
    dots <- rlang::quos(...)
    
    if (length(data@activated) == 0) {
      stop("activate your object using activate_mass_dataset first.\n")
    }
    
    x =
      slot(object = data, name = data@activated)
    
    x =
      drop_na(x, !!!dots)
    
    slot(object = data, name = data@activated) = x
    
    data = update_mass_dataset(data)
    
    return(data)
  }

#' @importFrom tidyr drop_na
#' @export
tidyr::drop_na
