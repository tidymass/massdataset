#' @method ggplot mass_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @export
ggplot.mass_dataset <- function(data = NULL, 
                                mapping = aes(), 
                                ..., 
                                environment = parent.frame()) {
  
  if (length(.data@activated) == 0) {
    stop("activate you object using activate_mass_dataset first.\n")
  }
  
  temp_slot =
    slot(object = .data, name = .data@activated)
  
  ggplot2::ggplot(data = temp_slot, 
                  mapping = mapping, 
                  environment = environment,
                  ...)
}

#' @importFrom ggplot2 ggplot
#' @export
ggplot2::ggplot

#' @importFrom ggplot2 aes
#' @export
ggplot2::aes