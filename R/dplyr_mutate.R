#' @method mutate mass_dataset
#' @importFrom rlang quos !!!
#' @importFrom dplyr mutate
#' @export
mutate.mass_dataset <- function(.data, ...) {
  dots <- quos(...)
  
  if (length(.data@activated) == 0) {
    stop("activate you object using activate_mass_dataset first.\n")
  }
  
  temp_slot =
    slot(object = .data, name = .data@activated)
  
  temp_slot =
    mutate(temp_slot, !!!dots)
  
  slot(object = .data, name = .data@activated) = temp_slot
  
  process_info = .data@process_info
  
  parameter <- new(
    Class = "tidymass_parameter",
    pacakge_name = "massdataset",
    function_name = "mutate()",
    parameter = list(parameter = rlang::expr_label(dots[[1]])),
    time = Sys.time()
  )
  
  if (all(names(process_info) != "mutate")) {
    process_info$mutate = parameter
  }else{
    process_info$mutate = c(process_info$mutate, parameter)  
  }
  
  .data@process_info = process_info
  
  return(.data)
}

#' @importFrom dplyr mutate
#' @export
dplyr::mutate

#' @importFrom dplyr transmute
#' @export
dplyr::transmute

#' @importFrom dplyr mutate_all
#' @export
dplyr::mutate_all

#' @importFrom dplyr mutate_at
#' @export
dplyr::mutate_at

#' @importFrom dplyr n
#' @export
dplyr::n