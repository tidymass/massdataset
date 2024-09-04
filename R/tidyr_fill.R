#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom tidyr fill
#' @export
fill.mass_dataset <-
  function(data, ..., .direction = c("down", "up", "downup", "updown")) {
    dots <- rlang::quos(...)
    .direction <- match.arg(.direction)
    if (length(data@activated) == 0) {
      stop("activate your object using activate_mass_dataset first.\n")
    }
    
    x <-
      slot(object = data, name = data@activated)
    
    x <-
      fill(x, !!!dots, .direction = .direction)
    
    slot(object = data, name = data@activated) = x

    return(data)
  }

#' @importFrom tidyr fill
#' @export
tidyr::fill
