#' @method filter mass_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr filter
#' @export
filter.mass_dataset <- function(.data, ..., .preserve = FALSE) {
  dots <- rlang::quos(...)
  
  if (length(.data@activated) == 0) {
    stop("activate you object using activate_mass_dataset first.\n")
  }
  
  x <-
    slot(object = .data, name = .data@activated)
  
  x <-
    filter(x, !!!dots, .preserve = .preserve)
  
  slot(object = .data, name = .data@activated) = x
  
  if (.data@activated == "annotation_table") {
    if(nrow(.data@annotation_table) > 0){
      annotation_table <- .data@annotation_table
      .data@variable_info <- .data@variable_info %>% 
        filter(variable_id %in% annotation_table$variable_id)
      .data@expression_data <- 
        .data@expression_data[.data@variable_info$variable_id, ,drop = FALSE]
    }
  }  
  
  if (.data@activated == "sample_info") {
    .data@expression_data <- 
      .data@expression_data[, x$sample_id,drop = FALSE]
  }
  
  if (.data@activated == "variable_info") {
    if(nrow(.data@annotation_table) > 0){
      variable_info <- .data@variable_info
      .data@annotation_table <- 
        .data@annotation_table %>% 
        dplyr::filter(variable_id %in% variable_info$variable_id)   
    }
    .data@expression_data <- 
      .data@expression_data[x$variable_id, ,drop = FALSE]
  }
  
  if (.data@activated == "expression_data") {
    .data@variable_info <- 
      .data@variable_info[match(rownames(x), .data@variable_info$variable_id),,drop = FALSE]
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
    process_info$filter <- parameter
  }else{
    process_info$filter <- c(process_info$filter, parameter)  
  }
  
  .data@process_info <- process_info
  
  return(.data)
}

#' @importFrom dplyr filter
#' @export
dplyr::filter
