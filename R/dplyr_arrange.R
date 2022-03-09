#' @method arrange mass_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr arrange
#' @export
arrange.mass_dataset <- 
  function(.data, ...) {
  dots <- rlang::quos(...)
  
  if (length(.data@activated) == 0) {
    stop("activate you object using activate_mass_dataset() first.\n")
  }
  
  x <-
    slot(object = .data, name = .data@activated)
  
  x <-
    arrange(x, !!!dots)
  
  slot(object = .data, name = .data@activated) = x
  
  if(.data@activated == "sample_info"){
    .data@expression_data = .data@expression_data[,x$sample_id, drop = FALSE]  
  }
  
  if(.data@activated == "variable_info"){
    .data@expression_data = .data@expression_data[x$variable_id,]  
  }
  
  if(.data@activated == "expression_data") {
    .data@variable_info <-
      .data@variable_info[match(rownames(.data@expression_data),
                                .data@variable_info$variable_id),]
  }
  
  return(.data)
}

#' @importFrom dplyr arrange
#' @export
dplyr::arrange

#' @importFrom dplyr desc
#' @export
dplyr::desc
