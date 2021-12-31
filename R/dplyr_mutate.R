#' @method mutate mass_dataset
#' @importFrom rlang quos !!!
#' @importFrom dplyr mutate
#' @export
mutate.mass_dataset <- function(.data, ...) {
  dots <- rlang::quos(...)
  
  if (length(.data@activated) == 0) {
    stop("activate you object using activate_mass_dataset first.\n")
  }
  
  temp_slot =
    slot(object = .data, name = .data@activated)
  
  temp_slot =
    mutate(temp_slot, !!!dots)
  
  slot(object = .data, name = .data@activated) = temp_slot
  
  if (.data@activated == "expression_data") {
    new_sample_id =
      setdiff(colnames(temp_slot), .data@sample_info$sample_id)
    if (length(new_sample_id) > 0) {
      new_sample_info =
        matrix(ncol = ncol(.data@sample_info),
               nrow = length(new_sample_id)) %>%
        as.data.frame()
      colnames(new_sample_info) = colnames(.data@sample_info)
      new_sample_info$sample_id = new_sample_id
      .data@sample_info = 
        rbind(.data@sample_info,
              new_sample_id)
      .data@expression_data = .data@expression_data[,.data@sample_info$sample_id]
    }
  }
  
  
  if (.data@activated == "sample_info") {
    if (ncol(temp_slot) > nrow(.data@sample_info_note)) {
      new_sample_info_note =
        data.frame(
          name = setdiff(colnames(temp_slot), .data@sample_info_note$name),
          meaning = setdiff(colnames(temp_slot), .data@sample_info_note$name)
        )
      .data@sample_info_note =
        rbind(.data@sample_info_note,
              new_sample_info_note)
      .data@sample_info = .data@sample_info[, .data@sample_info_note$name]
    }
  }
  
  if (.data@activated == "variable_info") {
    if (ncol(temp_slot) > nrow(.data@variable_info_note)) {
      new_variable_info_note =
        data.frame(
          name = setdiff(colnames(temp_slot), .data@variable_info_note$name),
          meaning = setdiff(colnames(temp_slot), .data@variable_info_note$name)
        )
      .data@variable_info_note =
        rbind(.data@variable_info_note,
              new_variable_info_note)
      .data@variable_info = .data@variable_info[, .data@variable_info_note$name]
    }
  }
  
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
  } else{
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