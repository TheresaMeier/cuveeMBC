test_that("Merging of vine copulas works - fixed structure", {

  # Generate vine copulas for testing
  rvs_level1 = rvinecopulib::rvine_structure(order = c(4,2,5,3,1), struct_array = list(
    c(5,3,1,1),
    c(1,1,3),
    c(3,5),
    2
  ))

  rvs_level2 = rvinecopulib::rvine_structure(order = c(1,3,2), struct_array = list(
    c(2,2),
    3
  ))

  bridge_var = 2
  rvs_level3 = merge_edges_fixed_full(rvs_level1, rvs_level2, bridge_var)

  rvs_level3_true = rvinecopulib::rvine_structure(order = c(4,9,14,15,11,13,10,6,8,12,5,1,3,2,7), struct_array = list(
    c(5,10,15,11,13,12,6,8,7,7,1,3,2,7),
    c(1,6,11,13,12,7,8,7,12,2,3,2,7),
    c(3,8,13,12,7,8,7,12,2,3,2,7),
    c(2,7,12,7,8,6,12,2,3,1,7)
  ))

  expect_s3_class(rvs_level3, "rvine_structure")
  expect_equal(rvs_level3$d, rvs_level1$d * rvs_level2$d)
  expect_equal(sort(rvs_level3$order),seq_len(rvs_level3$d))
  expect_true(all(rvs_level3$order >= 1))
  expect_true(all(rvs_level3$order <= rvs_level3$d))

  expect_equal(rvs_level3, rvs_level3_true)
})

test_that("Merge works for different bridge variables - fixed structure", {
  # Generate vine copulas for testing
  rvs_level1 = rvinecopulib::rvine_structure(order = c(2,1,3,4), struct_array = list(
    c(3,4,4),
    c(4,3),
    1
  ))

  rvs_level2 = rvinecopulib::rvine_structure(order = c(2,4,1,3,5,6), struct_array = list(
    c(3,5,6,6,6),
    c(6,6,5,5),
    c(5,3,3),
    c(1,1),
    4
  ))

  for (bridge_var in seq_len(rvs_level1$d)) {
    rvs_level3 <- merge_edges_fixed_full(rvs_level1, rvs_level2, bridge_var)

    expect_s3_class(rvs_level3, "rvine_structure")
    expect_equal(rvs_level3$d, rvs_level1$d * rvs_level2$d)
  }
})
