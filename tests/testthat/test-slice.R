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

test_that(desc = "slice",
          code = {
            ###expression_data
            object1 <- 
              activate_mass_dataset(object, what = "expression_data")
            object1 <-
              slice(.data = object1, 1)
            result1 <-
              check_mass_dataset(
                expression_data = object1@expression_data,
                sample_info = object1@sample_info,
                variable_info = object1@variable_info,
                sample_info_note = object1@sample_info_note,
                variable_info_note = object1@variable_info_note
              )
            testthat::expect_equal(object = result1, "all good.")
            
            ###expression_data2
            object1 <- 
              activate_mass_dataset(object, what = "expression_data")
            object1 <-
              slice(.data = object1, n())
            result1 <-
              check_mass_dataset(
                expression_data = object1@expression_data,
                sample_info = object1@sample_info,
                variable_info = object1@variable_info,
                sample_info_note = object1@sample_info_note,
                variable_info_note = object1@variable_info_note
              )
            testthat::expect_equal(object = result1, "all good.")
            
            ###expression_data3
            object1 <- 
              activate_mass_dataset(object, what = "expression_data")
            object1 <-
              slice(.data = object1, 2:n())
            result1 <-
              check_mass_dataset(
                expression_data = object1@expression_data,
                sample_info = object1@sample_info,
                variable_info = object1@variable_info,
                sample_info_note = object1@sample_info_note,
                variable_info_note = object1@variable_info_note
              )
            testthat::expect_equal(object = result1, "all good.")
            
            ###expression_data4
            object1 <- 
              activate_mass_dataset(object, what = "expression_data")
            object1 <-
              slice(.data = object1, -c(1:2))
            result1 <-
              check_mass_dataset(
                expression_data = object1@expression_data,
                sample_info = object1@sample_info,
                variable_info = object1@variable_info,
                sample_info_note = object1@sample_info_note,
                variable_info_note = object1@variable_info_note
              )
            testthat::expect_equal(object = result1, "all good.")
            
            
            
            
            
            
            
            ###sample_info
            object2 <- 
              activate_mass_dataset(object, what = "sample_info")
            object2 <-
              slice(.data = object2, 1)
            result2 <-
              check_mass_dataset(
                expression_data = object2@expression_data,
                sample_info = object2@sample_info,
                variable_info = object2@variable_info,
                sample_info_note = object2@sample_info_note,
                variable_info_note = object2@variable_info_note
              )
            testthat::expect_equal(object = result2, "all good.")
            
            ###sample_info2
            object2 <- 
              activate_mass_dataset(object, what = "sample_info")
            object2 <-
              slice(.data = object2, n())
            result2 <-
              check_mass_dataset(
                expression_data = object2@expression_data,
                sample_info = object2@sample_info,
                variable_info = object2@variable_info,
                sample_info_note = object2@sample_info_note,
                variable_info_note = object2@variable_info_note
              )
            testthat::expect_equal(object = result2, "all good.")
            
            ###sample_info3
            object2 <- 
              activate_mass_dataset(object, what = "sample_info")
            object2 <-
              slice(.data = object2, 2:n())
            result2 <-
              check_mass_dataset(
                expression_data = object2@expression_data,
                sample_info = object2@sample_info,
                variable_info = object2@variable_info,
                sample_info_note = object2@sample_info_note,
                variable_info_note = object2@variable_info_note
              )
            testthat::expect_equal(object = result2, "all good.")
            
            ###sample_info4
            object2 <- 
              activate_mass_dataset(object, what = "sample_info")
            object2 <-
              slice(.data = object2, -c(1:2))
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
            object2 <- 
              activate_mass_dataset(object, what = "variable_info")
            object2 <-
              slice(.data = object2, 1)
            result2 <-
              check_mass_dataset(
                expression_data = object2@expression_data,
                sample_info = object2@sample_info,
                variable_info = object2@variable_info,
                sample_info_note = object2@sample_info_note,
                variable_info_note = object2@variable_info_note
              )
            testthat::expect_equal(object = result2, "all good.")
            
            ###variable_info2
            object2 <- 
              activate_mass_dataset(object, what = "variable_info")
            object2 <-
              slice(.data = object2, n())
            result2 <-
              check_mass_dataset(
                expression_data = object2@expression_data,
                sample_info = object2@sample_info,
                variable_info = object2@variable_info,
                sample_info_note = object2@sample_info_note,
                variable_info_note = object2@variable_info_note
              )
            testthat::expect_equal(object = result2, "all good.")
            
            ###variable_info3
            object2 <- 
              activate_mass_dataset(object, what = "variable_info")
            object2 <-
              slice(.data = object2, 2:n())
            result2 <-
              check_mass_dataset(
                expression_data = object2@expression_data,
                sample_info = object2@sample_info,
                variable_info = object2@variable_info,
                sample_info_note = object2@sample_info_note,
                variable_info_note = object2@variable_info_note
              )
            testthat::expect_equal(object = result2, "all good.")
            
            ###variable_info4
            object2 <- 
              activate_mass_dataset(object, what = "variable_info")
            object2 <-
              slice(.data = object2, -c(1:2))
            result2 <-
              check_mass_dataset(
                expression_data = object2@expression_data,
                sample_info = object2@sample_info,
                variable_info = object2@variable_info,
                sample_info_note = object2@sample_info_note,
                variable_info_note = object2@variable_info_note
              )
            testthat::expect_equal(object = result2, "all good.")
            
          })









test_that(desc = "slice_head",
          code = {
            ###expression_data
            object1 <- 
              activate_mass_dataset(object, what = "expression_data")
            object1 <-
              slice_head(.data = object1, n = 2)
            result1 <-
              check_mass_dataset(
                expression_data = object1@expression_data,
                sample_info = object1@sample_info,
                variable_info = object1@variable_info,
                sample_info_note = object1@sample_info_note,
                variable_info_note = object1@variable_info_note
              )
            testthat::expect_equal(object = result1, "all good.")
            
            ###sample_info
            object2 <- 
              activate_mass_dataset(object, what = "sample_info")
            object2 <-
              slice_head(.data = object2, n = 3)
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
            object2 <- 
              activate_mass_dataset(object, what = "variable_info")
            object2 <-
              slice_head(.data = object2, n = 2)
            result2 <-
              check_mass_dataset(
                expression_data = object2@expression_data,
                sample_info = object2@sample_info,
                variable_info = object2@variable_info,
                sample_info_note = object2@sample_info_note,
                variable_info_note = object2@variable_info_note
              )
            testthat::expect_equal(object = result2, "all good.")
          })










test_that(desc = "slice_tail",
          code = {
            ###expression_data
            object1 <- 
              activate_mass_dataset(object, what = "expression_data")
            object1 <-
              slice_tail(.data = object1, n = 2)
            result1 <-
              check_mass_dataset(
                expression_data = object1@expression_data,
                sample_info = object1@sample_info,
                variable_info = object1@variable_info,
                sample_info_note = object1@sample_info_note,
                variable_info_note = object1@variable_info_note
              )
            testthat::expect_equal(object = result1, "all good.")
            
            ###sample_info
            object2 <- 
              activate_mass_dataset(object, what = "sample_info")
            object2 <-
              slice_tail(.data = object2, n = 3)
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
            object2 <- 
              activate_mass_dataset(object, what = "variable_info")
            object2 <-
              slice_tail(.data = object2, n = 2)
            result2 <-
              check_mass_dataset(
                expression_data = object2@expression_data,
                sample_info = object2@sample_info,
                variable_info = object2@variable_info,
                sample_info_note = object2@sample_info_note,
                variable_info_note = object2@variable_info_note
              )
            testthat::expect_equal(object = result2, "all good.")
          })




test_that(desc = "slice_sample",
          code = {
            ###expression_data
            object1 <- 
              activate_mass_dataset(object, what = "expression_data")
            object1 <-
              slice_sample(.data = object1, n = 2)
            result1 <-
              check_mass_dataset(
                expression_data = object1@expression_data,
                sample_info = object1@sample_info,
                variable_info = object1@variable_info,
                sample_info_note = object1@sample_info_note,
                variable_info_note = object1@variable_info_note
              )
            testthat::expect_equal(object = result1, "all good.")
            
            ###sample_info
            object2 <- 
              activate_mass_dataset(object, what = "sample_info")
            object2 <-
              slice_sample(.data = object2, n = 3)
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
            object2 <- 
              activate_mass_dataset(object, what = "variable_info")
            object2 <-
              slice_sample(.data = object2, n = 2)
            result2 <-
              check_mass_dataset(
                expression_data = object2@expression_data,
                sample_info = object2@sample_info,
                variable_info = object2@variable_info,
                sample_info_note = object2@sample_info_note,
                variable_info_note = object2@variable_info_note
              )
            testthat::expect_equal(object = result2, "all good.")
          })
