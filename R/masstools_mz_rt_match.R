#' @title mz_rt_match
#' @method mz_rt_match mass_dataset
#' @param data1 A mass_dataset object.
#' @param data2 A mass_dataset object.
#' @param mz.tol mz tolerance. default is 10 ppm.
#' @param rt.tol rt tolerance. default is 30 seconds.
#' @param rt.error.type RT match tolerance. abs (absolute) or relative.
#' @export
#' @rdname summary-mass_dataset
#' @importFrom masstools mz_rt_match
#' @return A merged mass_dataset object

setMethod(f = "mz_rt_match",
          signature(data1 = "mass_dataset",
                    data2 = "mass_dataset"),
          function (data1,
                    data2,
                    mz.tol = 10,
                    rt.tol = 30,
                    rt.error.type = c("abs", "relative")) {
            rt.error.type = match.arg(rt.error.type)
            match_result =
              masstools::mz_rt_match(
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
          })
