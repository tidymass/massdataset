
#' @method mz_rt_match mass_dataset
#' @param data1 data1
#' @param data2 data2
#' @param mz.tol mz.tol
#' @param rt.tol rt.tol
#' @param rt.error.type rt.error.type abs or relative
#' @export
#' @rdname mass_dataset-class
#' @importFrom tinytools mz_rt_match
#' @return mass_dataset object

setMethod(
  f = "mz_rt_match",
  signature = "mass_dataset",
  definition = function(data1,
                        data2,
                        mz.tol = 10,
                        rt.tol = 30,
                        rt.error.type = c("abs", "relative")) {
    rt.error.type = match.arg(rt.error.type)
    match_result = 
      mz_rt_match(
        data1 = as.matrix(data1@variable_info[, c("mz", "rt")]),
        data2 = as.matrix(data1@variable_info[, c("mz", "rt")]),
        mz.tol = mz.tol,
        rt.tol = rt.tol,
        rt.error.type = rt.error.type
      )
    
    match_result$variable_id1 = 
      data1@variable_info$variable_id[match_result$Index1]
    
    match_result$variable_id2 = 
      data1@variable_info$variable_id[match_result$Index2]
    
    match_result %>% 
      dplyr::select(variable_id1, variable_id2, dplyr::everything())
  }
)


#' @importFrom tinytools mz_rt_match
#' @export
tinytools::mz_rt_match
