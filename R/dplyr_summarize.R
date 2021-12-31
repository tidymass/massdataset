#' @method summarize mass_dataset
#' @importFrom rlang quos !!!
#' @importFrom dplyr summarize
#' @export
summarize.mass_dataset <- 
  function(.data, ...) {
  dots <- quos(...)
  
  if (length(.data@activated) == 0) {
    stop("activate you object using activate_mass_dataset first.\n")
  }
  
  x =
    slot(object = .data, name = .data@activated)
  
  x =
    summarize(x, !!!dots)
  
  return(x)
}


#' @method summarise mass_dataset
#' @importFrom rlang quos !!!
#' @importFrom dplyr summarise
#' @export
summarise.mass_dataset <- 
  function(.data, ...) {
    dots <- quos(...)
    
    if (length(.data@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    x =
      slot(object = .data, name = .data@activated)
    
    x =
      summarise(x, !!!dots)
    
    return(x)
  }


#' @importFrom dplyr summarize
#' @export
dplyr::summarize


#' @importFrom dplyr n
#' @export
dplyr::n





