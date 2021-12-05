#' @method select mass_dataset
#' @importFrom rlang quos !!!
#' @importFrom dplyr select
#' @export
select.mass_dataset <- 
  function(.data, ...) {
  dots <- quos(...)
  
  if (length(.data@activated) == 0) {
    stop("activate you object using activate_mass_dataset first.\n")
  }
  
  x =
    slot(object = .data, name = .data@activated)
  
  x =
    select(x, !!!dots)
  
  slot(object = .data, name = .data@activated) = x
  
  if(.data@activated == "expression_data"){
    .data@sample_info = .data@sample_info[match(colnames(x), .data@sample_info$sample_id),]
  }
  
  return(.data)
}

#' @importFrom dplyr select
#' @export
dplyr::select

#' @importFrom dplyr desc
#' @export
dplyr::desc
