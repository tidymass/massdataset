#' @method select mass_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr select
#' @export
select.mass_dataset <-
  function(.data, ...) {
    dots <- rlang::quos(...)
    
    if (length(.data@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    x <-
      slot(object = .data, name = .data@activated)
    
    x <-
      select(x, !!!dots)
    
    slot(object = .data, name = .data@activated) <- x
    
    if (.data@activated == "expression_data") {
      .data@sample_info <-
        .data@sample_info[match(colnames(x), .data@sample_info$sample_id),]
    }
    
    if (.data@activated == "sample_info") {
      ##we can not remove sample_id
      if(!"sample_id" %in% colnames(x)){
        stop("You can't remove sample_id.\n")
      }
      .data@sample_info_note <- .data@sample_info_note %>%
        dplyr::filter(name %in% colnames(x))
      .data <- update_sample_info(.data)
    }
    
    if (.data@activated == "variable_info") {
      ##we can not remove variable_id
      if(!"variable_id" %in% colnames(x)){
        stop("You can't remove variable_id\n")
      }
      .data@variable_info_note <- .data@variable_info_note %>% 
        dplyr::filter(name %in% colnames(x))
      .data <- update_variable_info(.data)
    }
    
    return(.data)
  }

#' @importFrom dplyr select
#' @export
dplyr::select

#' @importFrom dplyr desc
#' @export
dplyr::desc
