test_that("translate_tidymass_parameter returns a tidy data frame", {
  object <- make_test_mass_dataset()
  updated <- mutate_mean_intensity(object)

  parameter_df <- translate_tidymass_parameter(extract_process_info(updated)[[2]])

  testthat::expect_s3_class(parameter_df, "data.frame")
  testthat::expect_equal(
    colnames(parameter_df),
    c("pacakge_name", "function_name", "parameter", "time")
  )
  testthat::expect_true(any(parameter_df$function_name == "mutate_mean_intensity()"))
})

test_that("create_mass_dataset records its creation parameter", {
  object <- make_test_mass_dataset()

  process_info <- extract_process_info(object)

  testthat::expect_true("create_mass_dataset" %in% names(process_info))
  testthat::expect_s4_class(process_info$create_mass_dataset, "tidymass_parameter")
})
