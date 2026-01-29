test_that("reordering of data frames works", {
  # Generate random data
  set.seed(42)
  data <- data.frame(matrix(rnorm(400), ncol = 4))
  colnames(data) <- c("var1.loc1", "var1.loc2", "var2.loc1", "var2.loc2")

  # Test correct reordering
  expect_equal(reorder_dataset(data, direction = "variable-major"),
               data[, c("var1.loc1", "var1.loc2", "var2.loc1", "var2.loc2")])
  expect_equal(reorder_dataset(data, direction = "variable-major", order = c(2,1)),
               data[, c("var2.loc1", "var2.loc2", "var1.loc1", "var1.loc2")])
  expect_equal(reorder_dataset(data, direction = "location-major"),
               data[, c("var1.loc1", "var2.loc1", "var1.loc2", "var2.loc2")])
  expect_equal(reorder_dataset(data, direction = "location-major", order = c(2,1)),
               data[, c("var2.loc1", "var1.loc1", "var2.loc2", "var1.loc2")])

  expect_error(reorder_dataset(data, direction = "invalid-direction"),
               "Please select either 'variable-major' or 'location-major' ordering.")

  expect_error(reorder_dataset(data, direction = "variable-major", order = c("var1", "var2")))
})
