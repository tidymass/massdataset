#' @title show_missing_values
#' @description Show the missing value distributation.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param show_row_names show row names or not. see?ComplexHeatmap::Heatmap
#' @param show_column_names show column names or not.
#' see?ComplexHeatmap::Heatmap
#' @param row_names_gp row names gp, see?ComplexHeatmap
#' @param column_names_gp column names gp, see?ComplexHeatmap
#' @param column_names_rot column names rot see?ComplexHeatmap::Heatmap
#' @param cell_color Cell color.
#' @param row_names_side Row names side. left or right.
#' @param percentage percentage or not.
#' @param sample_na_cutoff Na cutoff for samples.
#' @param variable_na_cutoff Na cutoff for variables
#' @param only_outlier_samples Only show the outlier samples?
#' @param only_outlier_variables Only show the outlier variables?
#' @param return_as_ggplot Return plot as ggplot2 object?
#' @param ... Other parameters for ComplexHeatmap::Heatmap
#' @return A ggplot2 class object
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
#'                       row_names_side = "left",
#'                       percentage = TRUE,
#'                       sample_na_cutoff = 50,
#'                       variable_na_cutoff = 20)

show_missing_values <-
  function(object,
           show_row_names = FALSE,
           show_column_names = TRUE,
           row_names_gp = gpar(fontsize = 12),
           column_names_gp = gpar(fontsize = 12),
           column_names_rot,
           cell_color = "transparent",
           row_names_side = "right",
           percentage = FALSE,
           sample_na_cutoff = 50,
           variable_na_cutoff = 50,
           only_outlier_samples = FALSE,
           only_outlier_variables = FALSE,
           return_as_ggplot = FALSE,
           ...) {
    massdataset::check_object_class(object = object, class = "mass_dataset")
    if (missing(column_names_rot)) {
      column_names_rot <- 45
    }
    # browser()
    expression_data <-
      extract_expression_data(object)
    
    sample_na <-
      apply(expression_data, 2, function(x) {
        sum(is.na(x))
      })
    
    variable_na <-
      apply(expression_data, 1, function(x) {
        sum(is.na(x))
      })
    
    if (percentage) {
      sample_na <- sample_na * 100 / nrow(expression_data)
      variable_na <- variable_na * 100 / ncol(expression_data)
    }
    
    if (all(sample_na == 0) & all(variable_na == 0)) {
      plot <-
        ggplot() +
        theme_bw() +
        ggtitle(label = "No missing values")
      return(plot)
    }
    
    if (only_outlier_samples) {
      expression_data <-
        expression_data[, sample_na > sample_na_cutoff, drop = FALSE]
      sample_na <- sample_na[sample_na > sample_na_cutoff]
      if (length(sample_na) == 0) {
        plot <-
          ggplot() +
          theme_bw() +
          ggtitle(label = "No outlier samples")
        return(plot)
      }
    }
    
    if (only_outlier_variables) {
      expression_data <-
        expression_data[variable_na > variable_na_cutoff, , drop = FALSE]
      variable_na <- variable_na[variable_na > variable_na_cutoff]
      if (length(variable_na) == 0) {
        plot <-
          ggplot() +
          theme_bw() +
          ggtitle(label = "No outlier variables")
        return(plot)
      }
    }
    
    expression_data[!is.na(expression_data)] <- "2"
    expression_data[is.na(expression_data)] <- "1"
    colors <- structure(c("1", "2"), names = c("1", "2"))
    
    if (nrow(expression_data) <= 100 &
        ncol(expression_data) <= 100) {
      cell_color <- "black"
    }
    
    sample_color <-
      dplyr::case_when(sample_na >= sample_na_cutoff ~ "red",
                       sample_na < sample_na_cutoff ~ "black")
    
    variable_color <-
      dplyr::case_when(variable_na >= variable_na_cutoff ~ "red",
                       variable_na < variable_na_cutoff ~ "black")
    
    plot <-
      suppressMessages(
        ComplexHeatmap::Heatmap(
          row_title = "Variables",
          column_title = "Samples",
          row_names_gp = row_names_gp,
          column_names_gp = column_names_gp,
          as.matrix(expression_data),
          name = "MV",
          cluster_columns = FALSE,
          cluster_rows = FALSE,
          show_row_names = show_row_names,
          show_column_names = show_column_names,
          column_names_rot = 45,
          col = colors,
          border = TRUE,
          rect_gp = gpar(col = cell_color),
          show_heatmap_legend = FALSE,
          row_names_side = row_names_side,
          top_annotation =
            ComplexHeatmap::columnAnnotation(
              "MV" = ComplexHeatmap::anno_barplot(
                x = sample_na,
                gp = gpar(col = sample_color,
                          fill = sample_color)
              ),
              name = ifelse(percentage, "MV(%)", "MV number")
            ),
          right_annotation =
            ComplexHeatmap::rowAnnotation(
              "MV" = ComplexHeatmap::anno_barplot(
                x = variable_na,
                gp = gpar(col = variable_color,
                          fill = variable_color)
              ),
              name = ifelse(percentage, "MV(%)", "MV number")
            )
        )
      )
    
    if(return_as_ggplot){
      if(requireNamespace("ggplotify", quietly = TRUE)){
        plot <- ggplotify::as.ggplot(plot)   
      }else{
        message("Please install ggplotify package")
      }
    }
    return(plot)
  }



#' @title show_sample_missing_values
#' @description show missing values for each sample
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) A mass_dataset class object.
#' @param color_by which column (in sample_info) is used to color samples
#' @param order_by which column (in sample_info) is used to order samples
#' @param percentage show MV percentage? TRUE or FALSE.
#' @param show_x_text show x axis text or not? TRUE or FALSE
#' @param show_x_ticks show x ticks or not? TRUE or FALSE
#' @param desc descend sample order or not. TRUE or FALSE.
#' @return A ggplot2 object
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
#' object
#'
#' ##show missing values plot
#' show_sample_missing_values(object)
#' show_sample_missing_values(object, color_by = "class")
#' show_sample_missing_values(object, color_by = "class", order_by = "na")
#' show_sample_missing_values(object, color_by = "class", order_by = "na",
#'                            desc = TRUE)

show_sample_missing_values <-
  function(object,
           color_by,
           order_by,
           percentage = FALSE,
           show_x_text = TRUE,
           show_x_ticks = TRUE,
           desc = FALSE) {
    check_object_class(object = object, class = "mass_dataset")
    
    if (missing(color_by)) {
      color_by <- "no"
    } else{
      if (all(colnames(object@sample_info) != color_by)) {
        stop("no ", color_by, " in sample_info, please check.\n")
      }
    }
    
    if (missing(order_by)) {
      order_by <- "sample_id"
    } else{
      if (all(colnames(object@sample_info) != order_by) &
          order_by != "na") {
        warning("no ", order_by, " in sample_info, set it as sample_id\n")
        order_by <- "sample_id"
      }
    }
    
    expression_data <-
      extract_expression_data(object)
    
    sample_info <-
      extract_sample_info(object)
    
    na <-
      apply(expression_data, 2, function(x) {
        sum(is.na(x))
      })
    
    if (percentage) {
      na <- na * 100 / nrow(expression_data)
    }
    
    if (desc) {
      temp_data <-
        data.frame(sample_info,
                   na = na,
                   check.names = FALSE) %>%
        dplyr::arrange(desc(get(order_by))) %>%
        dplyr::mutate(sample_id = factor(sample_id,
                                         levels = sample_id))
      
    } else{
      temp_data <-
        data.frame(sample_info,
                   na = na,
                   check.names = FALSE) %>%
        dplyr::arrange(get(order_by)) %>%
        dplyr::mutate(sample_id = factor(sample_id,
                                         levels = sample_id))
    }
    
    plot <-
      temp_data %>%
      ggplot2::ggplot(aes(sample_id, na)) +
      guides(color = guide_legend(title = color_by),
             size = guide_legend(title = ifelse(
               percentage, "MV percentage (%)", "MV number"
             ))) +
      labs(x = "",
           y = ifelse(percentage, "MV percentage (%)", "MV number")) +
      scale_x_discrete(expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 1
        )
      )
    
    if (!show_x_text) {
      plot <-
        plot +
        theme(axis.text.x = element_blank()) +
        labs(x = "Samples")
    }
    
    if (!show_x_ticks) {
      plot <-
        plot +
        theme(axis.ticks.x = element_blank(),
              panel.grid = element_blank())
    }
    
    if (color_by == "no") {
      plot <-
        plot +
        ggplot2::geom_point()
    } else{
      plot <-
        plot +
        ggplot2::geom_point(aes(color = get(color_by)))
    }
    
    return(plot)
  }










#' @title show_variable_missing_values
#' @description show missing values for each variable
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object (required) mass_dataset class object.
#' @param color_by which column (in variable_info) is used to color variables
#' @param order_by which column (in variable_info) is used to order variables
#' @param percentage show MV percentage? TRUE or FALSE
#' @param show_x_text show x axis text or not? TRUE or FALSE
#' @param show_x_ticks show x ticks or not? TRUE or FALSE
#' @param desc descend sample order or not. TRUE or FALSE.
#' @return A ggplot2 object
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
#' object
#'
#' ##show missing values plot
#' show_variable_missing_values(object)
#' show_variable_missing_values(object, color_by = "mz")
#' library(ggplot2)
#' show_variable_missing_values(object, color_by = "rt") +
#'   ggplot2::scale_color_gradient(low = "skyblue", high = "red")
#'
#' show_variable_missing_values(object, color_by = "mz",
#'                              order_by = "na")
#' show_variable_missing_values(object, color_by = "mz",
#'                              order_by = "na",
#'                            desc = TRUE, percentage = TRUE)



show_variable_missing_values <-
  function(object,
           color_by,
           order_by,
           percentage = FALSE,
           show_x_text = FALSE,
           show_x_ticks = FALSE,
           desc = FALSE) {
    check_object_class(object = object, class = "mass_dataset")
    
    if (missing(color_by)) {
      color_by <- "no"
    } else{
      if (all(colnames(object@variable_info) != color_by)) {
        stop("no ", color_by, " in variable_info, please check.\n")
      }
    }
    
    if (missing(order_by)) {
      order_by <- "variable_id"
    } else{
      if (all(colnames(object@variable_info) != order_by) &
          order_by != "na") {
        stop("no ", order_by, " in variable_info, please check.\n")
      }
    }
    
    expression_data <-
      extract_expression_data(object)
    
    variable_info <-
      extract_variable_info(object)
    
    na <-
      apply(expression_data, 1, function(x) {
        sum(is.na(x))
      })
    
    if (percentage) {
      na <- na * 100 / ncol(expression_data)
    }
    
    if (desc) {
      temp_data <-
        data.frame(variable_info,
                   na = na,
                   check.names = FALSE) %>%
        dplyr::arrange(desc(get(order_by))) %>%
        dplyr::mutate(variable_id = factor(variable_id,
                                           levels = variable_id))
      
    } else{
      temp_data <-
        data.frame(variable_info,
                   na = na,
                   check.names = FALSE) %>%
        dplyr::arrange(get(order_by)) %>%
        dplyr::mutate(variable_id = factor(variable_id,
                                           levels = variable_id))
    }
    
    plot <-
      temp_data %>%
      ggplot2::ggplot(aes(variable_id, na)) +
      guides(color = guide_legend(title = color_by),
             size = guide_legend(title = ifelse(
               percentage, "MV percentage (%)", "MV number"
             ))) +
      scale_x_discrete(expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
      labs(x = "",
           y = ifelse(percentage, "MV percentage (%)", "MV number")) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(
              angle = 45,
              hjust = 1,
              vjust = 1
            ))
    
    if (!show_x_text) {
      plot <-
        plot +
        theme(axis.text.x = element_blank()) +
        labs(x = "Variables")
    }
    
    if (!show_x_ticks) {
      plot <-
        plot +
        theme(axis.ticks.x = element_blank(),
              panel.grid = element_blank())
    }
    
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
    
    return(plot)
  }
