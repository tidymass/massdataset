#' @title merge_mass_dataset
#' @description Merge two mass_dataset objects. More information can be found 
#' here \url{https://tidymass.github.io/massdataset/articles/process_info.html}
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param x (required) A mass_dataset class object.
#' @param y (required) A mass_dataset class object.
#' @param sample_direction How to merge samples, should be 
#' left, right, inner or full. See ?left_join
#' @param variable_direction How to merge variables, 
#' should be left, right, inner or full.
#' @param sample_by merge samples by what columns from sample_info.
#' @param variable_by merge variables by what columns from variable_info
#' @return A merged mass_dataset.
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#'
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'   )
#'
#' x = object[1:3, 5:7]
#' y = object[2:4, 6:8]
#'
#' ####full merge for samples and variables
#' z1 =
#' merge_mass_dataset(
#'   x = x,
#'   y = y,
#'   sample_direction = "full",
#'   variable_direction = "full"
#' )
#'
#' ####inner merge for samples and full merge for variables
#' z2 =
#'   merge_mass_dataset(
#'     x = x,
#'     y = y,
#'     sample_direction = "inner",
#'     variable_direction = "full"
#'   )
#'
#' extract_expression_data(x)
#' extract_expression_data(y)
#' extract_expression_data(z1)
#' extract_expression_data(z2)
#'
#' ######combine pos and neg
#' x = object[1:3, 5:7]
#' y = object[4:6, 6:8]
#'
#' z3 =
#'   merge_mass_dataset(
#'     x = x,
#'     y = y,
#'     sample_direction = "full",
#'     variable_direction = "full"
#'   )
#'
#' extract_expression_data(z3)

merge_mass_dataset <-
  function(x,
           y,
           sample_direction = c("left", "right", "full", "inner"),
           variable_direction = c("left", "right", "full", "inner"),
           sample_by = c("sample_id"),
           variable_by = c("variable_id", "mz", "rt")) {
    sample_direction <- match.arg(sample_direction)
    variable_direction <- match.arg(variable_direction)
    
    #####sample merge
    sample_info_x <- x@sample_info
    sample_info_y <- y@sample_info
    
    sample_info_note_x <- x@sample_info_note
    sample_info_note_y <- y@sample_info_note
    
    colnames(sample_info_y) <-
      colnames(sample_info_y) %>%
      lapply(function(x) {
        if (x %in% colnames(sample_info_y)) {
          if (!x %in% sample_by) {
            x = paste(x, 2, sep = "_")
          }
        }
        x
      }) %>%
      unlist()
    
    sample_info_note_y$name <- colnames(sample_info_y)
    
    ####left join
    if (sample_direction == "left") {
      sample_info <-
        sample_info_x %>%
        dplyr::left_join(sample_info_y, by = sample_by)
    }
    
    if (sample_direction == "right") {
      sample_info <-
        sample_info_x %>%
        dplyr::right_join(sample_info_y, by = sample_by)
    }
    
    if (sample_direction == "full") {
      sample_info <-
        sample_info_x %>%
        dplyr::full_join(sample_info_y, by = sample_by)
    }
    
    if (sample_direction == "inner") {
      sample_info <-
        sample_info_x %>%
        dplyr::inner_join(sample_info_y, by = sample_by)
    }
    
    sample_info_note <-
      rbind(sample_info_note_x,
            sample_info_note_y) %>%
      dplyr::distinct(name, .keep_all = TRUE)
    
    
    #####variable merge
    variable_info_x <- x@variable_info
    variable_info_y <- y@variable_info
    
    variable_info_note_x <- x@variable_info_note
    variable_info_note_y <- y@variable_info_note
    
    colnames(variable_info_y) <-
      colnames(variable_info_y) %>%
      lapply(function(x) {
        if (x %in% colnames(variable_info_y)) {
          if (!x %in% variable_by) {
            x = paste(x, 2, sep = "_")
          }
        }
        x
      }) %>%
      unlist()
    
    variable_info_note_y$name <- colnames(variable_info_y)
    
    ####left join
    if (variable_direction == "left") {
      variable_info <-
        variable_info_x %>%
        dplyr::left_join(variable_info_y,
                         by = variable_by)
    }
    
    if (variable_direction == "right") {
      variable_info <-
        variable_info_x %>%
        dplyr::right_join(variable_info_y, by = variable_by)
    }
    
    if (variable_direction == "full") {
      variable_info <-
        variable_info_x %>%
        dplyr::full_join(variable_info_y, by = variable_by)
    }
    
    if (variable_direction == "inner") {
      variable_info <-
        variable_info_x %>%
        dplyr::inner_join(variable_info_y, by = variable_by)
    }
    
    variable_info_note <-
      rbind(variable_info_note_x,
            variable_info_note_y) %>%
      dplyr::distinct(name, .keep_all = TRUE)
    
    ####expression_data
    expression_data_x <- x@expression_data
    expression_data_y <- y@expression_data
    
    expression_data <-
      expression_data_x %>%
      tibble::rownames_to_column(var = "variable_id") %>%
      dplyr::full_join(
        expression_data_y %>%
          tibble::rownames_to_column(var = "variable_id"),
        by = c("variable_id", intersect(
          colnames(expression_data_x),
          colnames(expression_data_y)
        ))
      ) %>%
      tibble::column_to_rownames(var = "variable_id")
    
    expression_data <-
      expression_data[variable_info$variable_id, sample_info$sample_id]
    
    
    ####annotation_table
    annotation_table_x <- x@annotation_table
    annotation_table_y <- y@annotation_table
    
    if(nrow(annotation_table_x) == 0 & nrow(annotation_table_y) == 0){
      annotation_table <- annotation_table_x
    }
    
    if(nrow(annotation_table_x) != 0 & nrow(annotation_table_y) == 0){
      annotation_table <- annotation_table_x
    }
    
    if(nrow(annotation_table_x) == 0 & nrow(annotation_table_y) != 0){
      annotation_table <- annotation_table_y
    }
    
    if(nrow(annotation_table_x) != 0 & nrow(annotation_table_y) != 0){
      annotation_table <- 
        rbind(annotation_table_x,
              annotation_table_y) %>% 
        dplyr::filter(variable_id %in% variable_info$variable_id)
    }
    
    object <- new(
      Class = "mass_dataset",
      expression_data = expression_data,
      ms2_data = data.frame(),
      sample_info = sample_info,
      variable_info = variable_info,
      sample_info_note = sample_info_note,
      variable_info_note = variable_info_note,
      process_info = c(x@process_info, y@process_info),
      version = massdataset_version, 
      annotation_table = annotation_table
    )
    
    ###add paramters
    ####add parameters
    process_info <- object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "merge_mass_dataset",
      parameter = list(
        sample_direction = sample_direction,
        variable_direction = variable_direction,
        sample_by = sample_by,
        variable_by = variable_by
      ),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "merge_mass_dataset")) {
      process_info$merge_mass_dataset <- parameter
    } else{
      process_info$merge_mass_dataset <- 
        c(process_info$merge_mass_dataset, parameter)
    }
    
    object@process_info <- process_info
    
    return(object)
  }
