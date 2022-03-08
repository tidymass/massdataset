expression_data <-
  as.data.frame(matrix(1:20, nrow = 5, ncol = 4))

colnames(expression_data) <-
  paste0("sample", 1:4)

rownames(expression_data) <-
  paste0("variable", 1:5)

sample_info <-
  data.frame(
    sample_id = colnames(expression_data),
    injection.order = 1:4,
    class = "QC",
    group = "case"
  )

variable_info <-
  data.frame(
    variable_id = rownames(expression_data),
    mz = 1:5,
    rt = 2:6
  )

object <-
  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info
  )

test_that(desc = "mutate_variable_na_number",
          code = {
            object1 <- 
              activate_mass_dataset(object, what = "expression_data")
            object1 <-
              mutate_variable_na_number(object = object1, 
                                      according_to_samples = object1@sample_info$sample_id)
            result1 <-
              check_mass_dataset(
                expression_data = object1@expression_data,
                sample_info = object1@sample_info,
                variable_info = object1@variable_info,
                sample_info_note = object1@sample_info_note,
                variable_info_note = object1@variable_info_note
              )
            testthat::expect_equal(object = result1, "all good.")
          })



test_that(desc = "mutate_sample_na_freq",
          code = {
            object1 <- 
              activate_mass_dataset(object, what = "expression_data")
            object1 <-
              mutate_variable_na_freq(object = object1, 
                                    according_to_samples = object1@sample_info$sample_id)
            result1 <-
              check_mass_dataset(
                expression_data = object1@expression_data,
                sample_info = object1@sample_info,
                variable_info = object1@variable_info,
                sample_info_note = object1@sample_info_note,
                variable_info_note = object1@variable_info_note
              )
            testthat::expect_equal(object = result1, "all good.")
          })


