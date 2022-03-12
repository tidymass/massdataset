#' @method add_column mass_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom tibble add_column
#' @export
add_column.mass_dataset <-
  function(.data,
           ...,
           .before = NULL,
           .after = NULL,
           .name_repair = c("check_unique", "unique", "universal", "minimal")) {
    .name_repair <- match.arg(.name_repair)
    dots <- rlang::quos(...)
    if (length(.data@activated) == 0) {
      stop("activate you object using activate_mass_dataset first.\n")
    }
    
    temp_slot <-
      slot(object = .data, name = .data@activated)
    
    temp_slot <-
      add_column(temp_slot,
                 !!!dots,
                 .before = .before,
                 .after = .after,
                 .name_repair = .name_repair)
    
    slot(object = .data, name = .data@activated) <- temp_slot
    
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
                new_sample_info)
        .data@expression_data <-
          .data@expression_data[, .data@sample_info$sample_id, drop = FALSE]
      }
    }
    
    if (.data@activated == "sample_info") {
      if (ncol(temp_slot) > nrow(.data@sample_info_note)) {
        new_sample_info_note =
          data.frame(
            name = setdiff(colnames(temp_slot), .data@sample_info_note$name),
            meaning = setdiff(colnames(temp_slot), .data@sample_info_note$name),
            check.names = FALSE
          )
        .data@sample_info_note <-
          rbind(.data@sample_info_note,
                new_sample_info_note)
        .data@sample_info <-
          .data@sample_info[, .data@sample_info_note$name, drop = FALSE]
      }
    }
    
    if (.data@activated == "variable_info") {
      if (ncol(temp_slot) > nrow(.data@variable_info_note)) {
        new_variable_info_note =
          data.frame(
            name = setdiff(colnames(temp_slot), .data@variable_info_note$name),
            meaning = setdiff(colnames(temp_slot), .data@variable_info_note$name),
            check.names = FALSE
          )
        .data@variable_info_note =
          rbind(.data@variable_info_note,
                new_variable_info_note)
        .data@variable_info <-
          .data@variable_info[, .data@variable_info_note$name, drop = FALSE]
      }
    }
    
    process_info <- .data@process_info
    
    internal_parameter <-
      purrr::map2(names(dots), dots, function(x, y) {
        y = rlang::expr_label(y)
        y = stringr::str_replace_all(y, "\\`", "") %>%
          stringr::str_replace("\\~", "")
        paste(x, y, sep = '=')
      })
    
    internal_parameter <-
      c(internal_parameter,
        .before = .before,
        .after = .after,
        .name_repair = .name_repair)
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "add_column()",
      parameter = internal_parameter,
      time = Sys.time()
    )
    
    if (all(names(process_info) != "add_column")) {
      process_info$add_column = parameter
    } else{
      process_info$add_column = c(process_info$add_column, parameter)
    }
    
    .data@process_info <- process_info

    return(.data)
  }

#' @importFrom tibble add_column
#' @export
tibble::add_column
