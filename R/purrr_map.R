#' #' @method map mass_dataset
#' #' @importFrom rlang quos !!!
#' #' @importFrom purrr map
#' #' @export
#' map.mass_dataset <- function(.x, .f, ...) {
#'   dots <- quos(...)
#'   
#'   if (length(.x@activated) == 0) {
#'     stop("activate you object using activate_mass_dataset first.\n")
#'   }
#'   
#'   temp_slot =
#'     slot(object = .x, name = .x@activated)
#'   
#'   temp_slot =
#'     map(temp_slot, .x = .x, .f = .f, !!!dots)
#'     
#'   slot(object = .x, name = .x@activated) = temp_slot
#'   
#'   return(.x)
#' }
#' 
#' #' @importFrom purrr map
#' #' @export
#' dplyr::mutate
