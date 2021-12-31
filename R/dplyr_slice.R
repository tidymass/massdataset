#' @method slice mass_dataset
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr slice any_of
#' @importFrom tidyselect eval_select
#' @importFrom vctrs vec_unique
#' @export
slice.mass_dataset <-
  function(.data, ..., .preserve = FALSE) {
    dots <- rlang::quos(...)
    
    if (length(.data@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    x =
      slot(object = .data, name = .data@activated)
    
    x =
      slice(x, !!!dots, .preserve = .preserve)
    
    slot(object = .data, name = .data@activated) = x
    
    if (.data@activated == "sample_info") {
      .data@expression_data = .data@expression_data[, x$sample_id, drop = FALSE]
    }
    
    if (.data@activated == "variable_info") {
      .data@expression_data = .data@expression_data[x$variable_id, , drop = FALSE]
    }
    
    if (.data@activated == "expression_data") {
      .data@variable_info = .data@variable_info[match(rownames(x), .data@variable_info$variable_id), , drop = FALSE]
    }
    
    process_info = .data@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "slice()",
      parameter = list(parameter = rlang::expr_label(dots[[1]])),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "slice")) {
      process_info$slice = parameter
    } else{
      process_info$slice = c(process_info$slice, parameter)
    }
    
    .data@process_info = process_info
    
    return(.data)
  }


#' @importFrom dplyr slice
#' @export
dplyr::slice


#' @method slice_head mass_dataset
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr slice_head any_of
#' @export
slice_head.mass_dataset <-
  function(.data, ..., n, prop) {
    # check_slice_dots(..., n = n, prop = prop)
    # size <- get_slice_size(n = n, prop = prop)
    # idx <- function(n)
    #   seq2(1, size(n))
    
    if (length(.data@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    x =
      slot(object = .data, name = .data@activated)
    
    # x = slice(x, idx(dplyr::n()))
    
    if (missing(n) & missing(prop)) {
      n = "missing"
      prop = "missing"
      x =
        slice_head(x, n = 1)
    }
    
    if (!missing(n)) {
      prop = "missing"
      x =
        slice_head(x, n = n)
    }
    
    if (missing(n) & !missing(prop)) {
      n = "missing"
      x =
        slice_head(x, prop = prop)
    }
    
    slot(object = .data, name = .data@activated) = x
    
    if (.data@activated == "sample_info") {
      .data@expression_data = .data@expression_data[, x$sample_id, drop = FALSE]
    }
    
    if (.data@activated == "variable_info") {
      .data@expression_data = .data@expression_data[x$variable_id, , drop = FALSE]
    }
    
    if (.data@activated == "expression_data") {
      .data@variable_info = .data@variable_info[match(rownames(x), .data@variable_info$variable_id), , drop = FALSE]
    }
    
    process_info = .data@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "slice_head()",
      parameter = list(n = n,
                       prop = prop),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "slice_head")) {
      process_info$slice_head = parameter
    } else{
      process_info$slice_head = c(process_info$slice_head, parameter)
    }
    
    .data@process_info = process_info
    
    return(.data)
  }


#' @importFrom dplyr slice_head
#' @export
dplyr::slice_head








#' @method slice_tail mass_dataset
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr slice_tail any_of
#' @export
slice_tail.mass_dataset <-
  function(.data, ..., n, prop) {
    # check_slice_dots(..., n = n, prop = prop)
    # size <- get_slice_size(n = n, prop = prop)
    # idx <- function(n)
    #   seq2(1, size(n))
    
    if (length(.data@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    x =
      slot(object = .data, name = .data@activated)
    
    # x = slice(x, idx(dplyr::n()))
    
    if (missing(n) & missing(prop)) {
      n = "missing"
      prop = "missing"
      x =
        slice_tail(x, n = 1)
    }
    
    if (!missing(n)) {
      prop = "missing"
      x =
        slice_tail(x, n = n)
    }
    
    if (missing(n) & !missing(prop)) {
      n = "missing"
      x =
        slice_tail(x, prop = prop)
    }
    
    slot(object = .data, name = .data@activated) = x
    
    if (.data@activated == "sample_info") {
      .data@expression_data = .data@expression_data[, x$sample_id, drop = FALSE]
    }
    
    if (.data@activated == "variable_info") {
      .data@expression_data = .data@expression_data[x$variable_id, , drop = FALSE]
    }
    
    if (.data@activated == "expression_data") {
      .data@variable_info = .data@variable_info[match(rownames(x), .data@variable_info$variable_id), , drop = FALSE]
    }
    
    process_info = .data@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "slice_tail()",
      parameter = list(n = n,
                       prop = prop),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "slice_tail")) {
      process_info$slice_tail = parameter
    } else{
      process_info$slice_tail = c(process_info$slice_tail, parameter)
    }
    
    .data@process_info = process_info
    
    return(.data)
  }


#' @importFrom dplyr slice_tail
#' @export
dplyr::slice_tail






#' @method slice_sample mass_dataset
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr slice_sample any_of
#' @export
slice_sample.mass_dataset <-
  function(.data,
           ...,
           n,
           prop,
           weight_by = NULL,
           replace = FALSE) {
    # check_slice_dots(..., n = n, prop = prop)
    # size <- get_slice_size(n = n, prop = prop)
    # idx <- function(n)
    #   seq2(1, size(n))
    
    if (length(.data@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    x =
      slot(object = .data, name = .data@activated)
    
    # x = slice(x, idx(dplyr::n()))
    
    if (missing(n) & missing(prop)) {
      n = "missing"
      prop = "missing"
      x =
        slice_sample(x,
                     n = 1,
                     weight_by = weight_by,
                     replace = FALSE)
    }
    
    if (!missing(n)) {
      prop = "missing"
      x =
        slice_sample(x,
                     n = n,
                     weight_by = weight_by,
                     replace = FALSE)
    }
    
    if (missing(n) & !missing(prop)) {
      n = "missing"
      x =
        slice_sample(x,
                     prop = prop,
                     weight_by = weight_by,
                     replace = FALSE)
    }
    
    slot(object = .data, name = .data@activated) = x
    
    if (.data@activated == "sample_info") {
      .data@expression_data = .data@expression_data[, x$sample_id, drop = FALSE]
    }
    
    if (.data@activated == "variable_info") {
      .data@expression_data = .data@expression_data[x$variable_id, , drop = FALSE]
    }
    
    if (.data@activated == "expression_data") {
      .data@variable_info = .data@variable_info[match(rownames(x), .data@variable_info$variable_id), , drop = FALSE]
    }
    
    process_info = .data@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "slice_sample()",
      parameter = list(
        n = n,
        prop = prop,
        weight_by = weight_by,
        replace = replace
      ),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "slice_sample")) {
      process_info$slice_sample = parameter
    } else{
      process_info$slice_sample = c(process_info$slice_sample, parameter)
    }
    
    .data@process_info = process_info
    
    return(.data)
  }


#' @importFrom dplyr slice_sample
#' @export
dplyr::slice_sample








#' @method slice_min mass_dataset
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr slice_min any_of
#' @export
slice_min.mass_dataset <-
  function(.data,
           order_by,
           ...,
           n,
           prop,
           with_ties = TRUE) {
    
    order_by <- enquo(order_by)
    
    if (length(.data@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    x =
      slot(object = .data, name = .data@activated)
    
    # x = slice(x, idx(dplyr::n()))
    
    if (missing(n) & missing(prop)) {
      n = "missing"
      prop = "missing"
      x =
        slice_min(x,
                  order_by = !! order_by,
                  n = 1,
                  with_ties = with_ties)
    }
    
    if (!missing(n)) {
      prop = "missing"
      x =
        slice_min(x,
                  order_by = !! order_by,
                  n = n,
                  with_ties = with_ties)
    }
    
    if (missing(n) & !missing(prop)) {
      n = "missing"
      x =
        slice_min(x,
                  order_by = !! order_by,
                  prop = prop,
                  with_ties = with_ties)
    }
    
    slot(object = .data, name = .data@activated) = x
    
    if (.data@activated == "sample_info") {
      .data@expression_data = .data@expression_data[, x$sample_id, drop = FALSE]
    }
    
    if (.data@activated == "variable_info") {
      .data@expression_data = .data@expression_data[x$variable_id, , drop = FALSE]
    }
    
    if (.data@activated == "expression_data") {
      .data@variable_info = .data@variable_info[match(rownames(x), .data@variable_info$variable_id), , drop = FALSE]
    }
    
    process_info = .data@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "slice_min()",
      parameter = list(
        order_by = rlang::expr_label(order_by),
        n = n,
        prop = prop,
        with_ties = with_ties
      ),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "slice_min")) {
      process_info$slice_min = parameter
    } else{
      process_info$slice_min = c(process_info$slice_min, parameter)
    }
    
    .data@process_info = process_info
    
    return(.data)
  }


#' @importFrom dplyr slice_min
#' @export
dplyr::slice_min










#' @method slice_max mass_dataset
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr slice_max any_of
#' @export
slice_max.mass_dataset <-
  function(.data,
           order_by,
           ...,
           n,
           prop,
           with_ties = TRUE) {
    
    order_by <- enquo(order_by)
    
    if (length(.data@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    x =
      slot(object = .data, name = .data@activated)
    
    # x = slice(x, idx(dplyr::n()))
    
    if (missing(n) & missing(prop)) {
      n = "missing"
      prop = "missing"
      x =
        slice_max(x,
                  order_by = !! order_by,
                  n = 1,
                  with_ties = with_ties)
    }
    
    if (!missing(n)) {
      prop = "missing"
      x =
        slice_max(x,
                  order_by = !! order_by,
                  n = n,
                  with_ties = with_ties)
    }
    
    if (missing(n) & !missing(prop)) {
      n = "missing"
      x =
        slice_max(x,
                  order_by = !! order_by,
                  prop = prop,
                  with_ties = with_ties)
    }
    
    slot(object = .data, name = .data@activated) = x
    
    if (.data@activated == "sample_info") {
      .data@expression_data = .data@expression_data[, x$sample_id, drop = FALSE]
    }
    
    if (.data@activated == "variable_info") {
      .data@expression_data = .data@expression_data[x$variable_id, , drop = FALSE]
    }
    
    if (.data@activated == "expression_data") {
      .data@variable_info = .data@variable_info[match(rownames(x), .data@variable_info$variable_id), , drop = FALSE]
    }
    
    process_info = .data@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "slice_max()",
      parameter = list(
        order_by = rlang::expr_label(order_by),
        n = n,
        prop = prop,
        with_ties = with_ties
      ),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "slice_max")) {
      process_info$slice_max = parameter
    } else{
      process_info$slice_max = c(process_info$slice_max, parameter)
    }
    
    .data@process_info = process_info
    
    return(.data)
  }


#' @importFrom dplyr slice_max
#' @export
dplyr::slice_max