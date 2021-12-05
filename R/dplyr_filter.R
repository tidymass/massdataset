#' @method filter mass_dataset
#' @importFrom rlang quos !!!
#' @importFrom dplyr filter
#' @export
filter.mass_dataset <- function(.data, ..., .preserve = FALSE) {
  dots <- quos(...)
  
  if (length(.data@activated) == 0) {
    stop("activate you object using activate_mass_dataset first.\n")
  }
  
  x =
    slot(object = .data, name = .data@activated)
  
  x =
    filter(x, !!!dots, .preserve = .preserve)
  
  slot(object = .data, name = .data@activated) = x
  
  if (.data@activated == "sample_info") {
    .data@expression_data = .data@expression_data[, x$sample_id]
  }
  
  if (.data@activated == "variable_info") {
    .data@expression_data = .data@expression_data[x$variable_id, ]
  }
  
  if (.data@activated == "expression_data") {
    .data@variable_info = .data@variable_info[rownames(x), .data@variable_info$variable_id]
  }
  
  process_info = .data@process_info
  
  parameter <- new(
    Class = "tidymass_parameter",
    pacakge_name = "massdataset",
    function_name = "filter()",
    parameter = list(parameter = rlang::expr_label(dots[[1]])),
    time = Sys.time()
  )
  
  if (all(names(process_info) != "filter")) {
    process_info$filter = parameter
  }else{
    process_info$filter = c(process_info$filter, parameter)  
  }
  
  .data@process_info = process_info
  
  return(.data)
}

#' @importFrom dplyr filter
#' @export
dplyr::filter
