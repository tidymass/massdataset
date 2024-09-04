#' @method glimpse mass_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr glimpse
#' @export
glimpse.mass_dataset <-
  function(x, width, ...) {
    if (length(x@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    x <-
      slot(object = x, name = x@activated)
    dplyr::glimpse(x)
  }

#' @importFrom dplyr glimpse
#' @export
dplyr::glimpse
