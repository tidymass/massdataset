#' Create Intensity Plot for a Variable in a mass_dataset Object
#'
#' This function generates an intensity plot for a specific variable in a mass_dataset object. 
#' You can specify the variable either by its ID or its index. Additionally, you can customize 
#' the color of data points based on a specific attribute and order the samples accordingly.
#'
#' @param object A mass_dataset object.
#' @param variable_id A character string specifying the ID of the variable of interest. 
#'   If provided, 'variable_index' will be ignored.
#' @param variable_index An integer specifying the index of the variable of interest.
#'   If not provided, you can use 'variable_id' to specify the variable by its ID.
#' @param color_by A character string specifying the attribute to color data points. 
#'   Default is "no", meaning no coloring.
#' @param order_by A character string specifying the attribute to order the samples. 
#'   Default is "sample_id". If set to "na", no specific order is applied.
#' @param desc A logical value indicating whether to order the samples in descending order.
#'   Default is FALSE.
#' @param interactive A logical value indicating whether to create an interactive plot using plotly. 
#'   Default is TRUE.
#'
#' @return A ggplot2 or plotly plot object.
#'
#' @author Xiaotao Shen <shenxt1990@outlook.com>
#' @export
#' @examples
#' data("expression_data")
#' data("sample_info")
#' data("sample_info_note")
#' data("variable_info")
#' data("variable_info_note")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'     sample_info_note = sample_info_note,
#'     variable_info_note = variable_info_note
#'   )
#' object
#'
#' intensity_plot(object = object, variable_index = 1)
#' intensity_plot(object = object, variable_index = 1, color_by = "class")

intensity_plot <-
  function(object,
           variable_id,
           variable_index,
           color_by,
           order_by,
           desc = FALSE,
           interactive = TRUE) {
    check_object_class(object = object, class = "mass_dataset")
    
    if (missing(variable_id) & missing(variable_index)) {
      stop("provide variable_id or variable_index.\n")
    }
    
    if (!missing(variable_id)) {
      if (!variable_id %in% object@variable_info$variable_id) {
        stop(variable_id, " is not in object.\n")
      }
    } else{
      variable_id = object@variable_info$variable_id[variable_index]
      if (is.na(variable_id)) {
        stop(variable_index, " is out of you variable_info range.\n")
      }
    }
    
    int <-
      as.numeric(object@expression_data[variable_id, ])
    
    temp_data <-
      data.frame(object@sample_info,
                 int,
                 check.names = FALSE)
    
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
      if (all(colnames(object@sample_info) != order_by) &
          order_by != "na") {
        stop("no ", order_by, " in sample_info, please check.\n")
      }
    }
    
    
    if (desc) {
      temp_data =
        temp_data %>%
        dplyr::arrange(desc(get(order_by))) %>%
        dplyr::mutate(sample_id = factor(sample_id,
                                         levels = sample_id))
      
    } else{
      temp_data =
        temp_data %>%
        dplyr::arrange(get(order_by)) %>%
        dplyr::mutate(sample_id = factor(sample_id,
                                         levels = sample_id))
    }
    
    
    plot <-
      temp_data %>%
      ggplot2::ggplot(aes(sample_id, int)) +
      guides(color = guide_legend(title = color_by)) +
      labs(x = "",
           y = "Intensity") +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 1
        )
      )
    
    if (color_by == "no") {
      plot <-
        plot +
        ggplot2::geom_point()
    } else{
      plot <-
        plot +
        ggplot2::geom_point(aes(color = get(color_by))) +
        guides(color = guide_legend(title = color_by))
    }
    
    if (interactive) {
      if(requireNamespace("plotly", quietly = TRUE)){
        plot <- plotly::ggplotly(plot) 
      }else{
        message("Please install plotly package first.")
      }
    }
    
    return(plot)
  }
