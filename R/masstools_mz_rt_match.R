#' @method mz_rt_match mass_dataset
#' @docType methods
#' @importFrom masstools mz_rt_match
#' @export

mz_rt_match.mass_dataset <-
  function(data1,
           data2,
           mz.tol = 10,
           rt.tol = 30,
           rt.error.type = c("abs", "relative")) {
    rt.error.type = match.arg(rt.error.type)
    match_result =
      masstools::mz_rt_match(
        data1 = data1@variable_info[, c("mz", "rt")],
        data2 = data1@variable_info[, c("mz", "rt")],
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

#' @importFrom masstools mz_rt_match
#' @export
masstools::mz_rt_match
