test_that("adjust_confounder preserves structure and records process info", {
  object <- make_test_mass_dataset(n_variables = 6, n_samples = 6)
  object@sample_info$batch <- rep(c(1, 2), length.out = nrow(object@sample_info))
  object@sample_info$age <- seq_len(nrow(object@sample_info)) + 20
  object <- update_sample_info(object)

  adjusted <- adjust_confounder(object, confounder_name_list = c("batch", "age"))

  expect_valid_mass_dataset(adjusted)
  testthat::expect_equal(dim(adjusted@expression_data), dim(object@expression_data))
  testthat::expect_true("adjust_confounder" %in% names(adjusted@process_info))
  testthat::expect_false(identical(adjusted@expression_data, object@expression_data))
})

test_that("adjust_confounder errors for missing confounders", {
  object <- make_test_mass_dataset()

  testthat::expect_error(
    adjust_confounder(object, confounder_name_list = "missing_column")
  )
})
