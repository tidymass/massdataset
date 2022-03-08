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

new_sample_info <-
  data.frame(sample_id = sample_info$sample_id,
             new = "a")

new_variable_info <-
  data.frame(variable_id = c(variable_info$variable_id, "v6"),
             new = 20)

object <-
  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info
  )

test_that(desc = "left_join",
          code = {
            ###sample_info
            object2 <- 
              activate_mass_dataset(object, what = "sample_info")
            object2 <-
              left_join(x = object2, y = new_sample_info, by = "sample_id")
            result2 <-
              check_mass_dataset(
                expression_data = object2@expression_data,
                sample_info = object2@sample_info,
                variable_info = object2@variable_info,
                sample_info_note = object2@sample_info_note,
                variable_info_note = object2@variable_info_note
              )
            testthat::expect_equal(object = result2, "all good.")
            
            ###variable_info
            object3 <- 
              activate_mass_dataset(object, what = "variable_info")
            object3 <-
              left_join(x = object3, y = new_variable_info, by = "variable_id")
            result3 <-
              check_mass_dataset(
                expression_data = object3@expression_data,
                sample_info = object3@sample_info,
                variable_info = object3@variable_info,
                sample_info_note = object3@sample_info_note,
                variable_info_note = object3@variable_info_note
              )
            testthat::expect_equal(object = result3, "all good.")
          })
