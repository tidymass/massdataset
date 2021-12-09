#' @title Add NA number for each feature to variable_info
#' @description Add NA number for each feature to variable_info
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param show_row_names show_row_names. see?ComplexHeatmap::Heatmap
#' @param show_column_names show_column_names see?ComplexHeatmap::Heatmap
#' @param column_names_rot column_names_rot see?ComplexHeatmap::Heatmap
#' @param cell_color cell color
#' @param row_names_side row names side
#' @param ... Other parameters for ComplexHeatmap::Heatmap
#' @return A ggplot2 object
#' @export
#' @examples
#' library(massdataset)
#' library(plyr)
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
#' object
#'
#' ##show missing values plot
#' show_missing_values(object)
#'
#' show_missing_values(object[1:10,], cell_color = "white")
#'
#' ###only show subject samples
#' object %>%
#'   activate_mass_dataset(what = "sample_info") %>%
#'   filter(class == "Subject") %>%
#'   show_missing_values()
#'
#' ###only show QC samples
#' object %>%
#'   activate_mass_dataset(what = "expression_data") %>%
#'   dplyr::select(contains("QC")) %>%
#'   show_missing_values()
#'
#' ###only show features with mz < 100
#' object %>%
#'   activate_mass_dataset(what = "variable_info") %>%
#'   dplyr::filter(mz < 100) %>%
#'   show_missing_values(cell_color = "white",
#'                       show_row_names = TRUE,
#'                       row_names_side = "left")

show_missing_values =
  function(object,
           show_row_names = FALSE,
           show_column_names = TRUE,
           column_names_rot,
           cell_color = "transparent",
           row_names_side = "right",
           ...) {
    check_object_class(object = object, class = "mass_dataset")
    if (missing(column_names_rot)) {
      column_names_rot = 45
    }
    
    expression_data = object@expression_data
    
    expression_data[!is.na(expression_data)] = "2"
    expression_data[is.na(expression_data)] = "1"
    colors = structure(c("1", "2"), names = c("1", "2"))
    plot =
      suppressMessages(
        ComplexHeatmap::Heatmap(
          as.matrix(expression_data),
          name = "MV",
          cluster_columns = FALSE,
          cluster_rows = FALSE,
          show_row_names = show_row_names,
          show_column_names = show_column_names,
          column_names_rot = 45,
          col = colors,
          rect_gp = gpar(col = cell_color),
          show_heatmap_legend = FALSE,
          row_names_side = row_names_side
          
          # ...
        )
      )
    
    plot = ggplotify::as.ggplot(plot)
    
    return(plot)
  }








#' @title Add NA number for each feature to variable_info
#' @description Add NA number for each feature to variable_info
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param show_row_names show_row_names. see?ComplexHeatmap::Heatmap
#' @param show_column_names show_column_names see?ComplexHeatmap::Heatmap
#' @param column_names_rot column_names_rot see?ComplexHeatmap::Heatmap
#' @param cell_color cell color
#' @param row_names_side row names side
#' @param ... Other parameters for ComplexHeatmap::Heatmap
#' @return A ggplot2 object
#' @export
#' @examples
#' library(massdataset)
#' library(plyr)
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
#' object
#' 
#' ##show missing values plot
#' show_sample_missing_values(object)
#' show_sample_missing_values(object, color_by = "class")
#' show_sample_missing_values(object, color_by = "class", order_by = "na")
#' show_sample_missing_values(object, color_by = "class", order_by = "na",
#'                            desc = TRUE)


show_sample_missing_values =
  function(object,
           color_by,
           order_by,
           frequency = FALSE,
           desc = FALSE) {
    check_object_class(object = object, class = "mass_dataset")
    
    if (missing(color_by)) {
      color_by = "no"
    } else{
      if (all(colnames(object@sample_info) != color_by)) {
        stop("no ", color_by, " in sample_info, please check.\n")
      }
    }
    
    if (missing(order_by)) {
      order_by = "sample_id"
    } else{
      if (all(colnames(object@sample_info) != order_by) & order_by != "na") {
        stop("no ", order_by, " in sample_info, please check.\n")
      }
    }
    
    expression_data = object@expression_data
    sample_info = object@sample_info
    
    na =
      apply(expression_data, 2, function(x) {
        sum(is.na(x))
      })
    
    if (frequency) {
      na = na * 100 / nrow(expression_data)
    }
    
    if(desc){
      temp_data = 
        data.frame(sample_info, na = na) %>% 
        dplyr::arrange(desc(get(order_by))) %>%
        dplyr::mutate(sample_id = factor(sample_id,
                                         levels = sample_id))
        
    }else{
      temp_data = 
        data.frame(sample_info, na = na) %>% 
        dplyr::arrange(get(order_by)) %>%
        dplyr::mutate(sample_id = factor(sample_id,
                                         levels = sample_id))
    }
   
    
    plot =
      temp_data %>%
      ggplot2::ggplot(aes(sample_id, na)) +
      guides(color = guide_legend(title = color_by),
             size = guide_legend(title = ifelse(frequency, "MV percentage (%)", "MV number"))) +
      labs(x = "",
           y = ifelse(frequency, "MV percentage (%)", "MV number")) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            axis.text.x = element_text(
              angle = 45,
              hjust = 1,
              vjust = 1
            ))
    
    if (color_by == "no") {
      plot =
        plot +
        ggplot2::geom_point(aes(size = na))
    } else{
      plot =
        plot +
        ggplot2::geom_point(aes(size = na, color = get(color_by)))
    }
    
    return(plot)
  }
