#' Match Features by m/z and Retention Time
#'
#' @param data1 A `mass_dataset` object.
#' @param data2 A second `mass_dataset` object or compatible structure.
#' @param mz.tol Numeric m/z tolerance for matching.
#' @param rt.tol Numeric retention time tolerance for matching.
#' @param rt.error.type Retention time error type.
#' @export
match_mz_rt <- function(data1,
                        data2,
                        mz.tol = 10,
                        rt.tol = 30,
                        rt.error.type = c("abs", "relative")) {
  UseMethod("match_mz_rt")
}

#' @method match_mz_rt mass_dataset
#' @docType methods
#' @export

match_mz_rt.mass_dataset <-
  function(data1,
           data2,
           mz.tol = 10,
           rt.tol = 30,
           rt.error.type = c("abs", "relative")) {
    rt.error.type = match.arg(rt.error.type)
    match_result =
      match_mz_rt(
        data1 = data1@variable_info[, c("mz", "rt")],
        data2 = data2@variable_info[, c("mz", "rt")],
        mz.tol = mz.tol,
        rt.tol = rt.tol,
        rt.error.type = rt.error.type
      )
    
    match_result$variable_id1 =
      data1@variable_info$variable_id[match_result$Index1]
    
    match_result$variable_id2 =
      data2@variable_info$variable_id[match_result$Index2]
    
    match_result %>%
      dplyr::select(variable_id1, variable_id2, dplyr::everything())
  }
