# #' @title add_column
# #' @description add_column. This function is from tibble.
# #' @author Xiaotao Shen
# #' \email{shenxt1990@@outlook.com}
# #' @importFrom tibble add_column
# #' @include mass_dataset_class.R
# #' @param .data mass_data class
# #' @param ... dynamic-dots Name-value pairs, passed on to tibble().
# #' All values must have the same size of .data or size 1.
# #' @param .before One-based column index or column name where to add
# #' the new columns, default: after last column.
# #' @param .after One-based column index or column name where to add
# #' the new columns, default: after last column.
# #' @param .name_repair Treatment of problematic column names:
# #' "minimal": No name repair or checks, beyond basic existence,
# #' "unique": Make sure names are unique and not empty,
# #' "check_unique": (default value), no name repair, but check they are unique,
# #' "universal": Make the names unique and syntactic
# #' a function: apply custom name repair (e.g., .name_repair = make.names for
# #' names in the style of base R).
# #' A purrr-style anonymous function, see rlang::as_function()
# #' This argument is passed on as repair to vctrs::vec_as_names().
# #' See there for more details on these terms and the strategies
# #' used to enforce them.
# #' @return A mass_dataset class object
# #' @export
# 
# setGeneric(name = "add_column")
# 
# #' @title add_column
# #' @method add_column mass_dataset
# #' @param .data mass_data class
# #' @param ... dynamic-dots Name-value pairs, passed on to tibble().
# #' All values must have the same size of .data or size 1.
# #' @param .before One-based column index or column name where to add
# #' the new columns, default: after last column.
# #' @param .after One-based column index or column name where to add
# #' the new columns, default: after last column.
# #' @param .name_repair Treatment of problematic column names:
# #' "minimal": No name repair or checks, beyond basic existence,
# #' "unique": Make sure names are unique and not empty,
# #' "check_unique": (default value), no name repair, but check they are unique,
# #' "universal": Make the names unique and syntactic
# #' a function: apply custom name repair (e.g., .name_repair = make.names for
# #' names in the style of base R).
# #' A purrr-style anonymous function, see rlang::as_function()
# #' This argument is passed on as repair to vctrs::vec_as_names().
# #' See there for more details on these terms and the strategies
# #' used to enforce them.
# #' @include mass_dataset_class.R
# #' @export
# #' @return mass_dataset class
# 
# setMethod(f = "add_column",
#           signature("mass_dataset"),
#           function(.data,
#                    ...,
#                    .before = NULL,
#                    .after = NULL,
#                    .name_repair = c("check_unique",
#                                     "unique",
#                                     "universal",
#                                     "minimal")) {
#             .name_repair <- match.arg(.name_repair)
#             if (length(.data@activated) == 0) {
#               stop("activate you object using activate_mass_dataset first.\n")
#             }
# 
#             if (!.data@activated %in% c("expression_data",
#                                         "sample_info",
#                                         "variable_info")) {
#               stop("activate should be one of 'expression_data',
#                    'sample_info' and 'variable_info'")
#             }
# 
#             temp_slot <-
#               slot(object = .data, name = .data@activated)
# 
#             dots <- rlang::quos(...)
# 
#             temp_slot <-
#               tibble::add_column(
#                 temp_slot,
#                 !!!dots,
#                 .before = .before,
#                 .after = .after,
#                 .name_repair = .name_repair
#               )
# 
#             slot(object = .data, name = .data@activated) <-
#               temp_slot
# 
#             if (.data@activated == "expression_data") {
#               new_sample_id =
#                 setdiff(colnames(temp_slot), .data@sample_info$sample_id)
#               if (length(new_sample_id) > 0) {
#                 new_sample_info =
#                   matrix(ncol = ncol(.data@sample_info),
#                          nrow = length(new_sample_id)) %>%
#                   as.data.frame()
#                 colnames(new_sample_info) = colnames(.data@sample_info)
#                 new_sample_info$sample_id = new_sample_id
#                 .data@sample_info =
#                   rbind(.data@sample_info,
#                         new_sample_info)
#                 .data@expression_data <-
#                   .data@expression_data[, .data@sample_info$sample_id, drop = FALSE]
#               }
#             }
# 
#             if (.data@activated == "sample_info") {
#               if (ncol(temp_slot) > nrow(.data@sample_info_note)) {
#                 new_sample_info_note =
#                   data.frame(
#                     name = setdiff(colnames(temp_slot), .data@sample_info_note$name),
#                     meaning = setdiff(colnames(temp_slot), .data@sample_info_note$name),
#                     check.names = FALSE
#                   )
#                 .data@sample_info_note <-
#                   rbind(.data@sample_info_note,
#                         new_sample_info_note)
#                 .data@sample_info <-
#                   .data@sample_info[, .data@sample_info_note$name, drop = FALSE]
#               }
#             }
# 
#             if (.data@activated == "variable_info") {
#               if (ncol(temp_slot) > nrow(.data@variable_info_note)) {
#                 new_variable_info_note =
#                   data.frame(
#                     name = setdiff(colnames(temp_slot), .data@variable_info_note$name),
#                     meaning = setdiff(colnames(temp_slot), .data@variable_info_note$name),
#                     check.names = FALSE
#                   )
#                 .data@variable_info_note =
#                   rbind(.data@variable_info_note,
#                         new_variable_info_note)
#                 .data@variable_info <-
#                   .data@variable_info[, .data@variable_info_note$name, drop = FALSE]
#               }
#             }
# 
# 
#             process_info <- .data@process_info
# 
#             internal_parameter <-
#               purrr::map2(names(dots), dots, function(x, y) {
#                 y = rlang::expr_label(y)
#                 y = stringr::str_replace_all(y, "\\`", "") %>%
#                   stringr::str_replace("\\~", "")
#                 paste(x, y, sep = '=')
#               })
#             names(internal_parameter) <- "dots"
#             parameter <- new(
#               Class = "tidymass_parameter",
#               pacakge_name = "massdataset",
#               function_name = "mutate()",
#               parameter = c(
#                 internal_parameter,
#                 .before = .before,
#                 .after = .after,
#                 .name_repair = .name_repair
#               ),
#               time = Sys.time()
#             )
# 
#             if (all(names(process_info) != "add_column")) {
#               process_info$add_column = parameter
#             } else{
#               process_info$add_column = c(process_info$add_column, parameter)
#             }
# 
#             .data@process_info <- process_info
# 
#             return(.data)
#           })
