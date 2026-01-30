test_that("spatial grid derivation works", {
  mask <- get_spatial_mask(4, 6)

  # Correct class and dimensions
  expect_s4_class(mask, "dgCMatrix")
  expect_equal(dim(mask), c(4*6, 4*6))

  # Symmetry and no self-loops
  expect_equal(mask, Matrix::t(mask))
  expect_equal(Matrix::diag(mask), rep(0, nrow(mask)))

  # Correct neighbor counts
  # Helper to compute 1D index
  idx <- function(r, c) (r - 1) * 4 + c

  # Corner cell (1,1)
  expect_equal(sum(mask[idx(1, 1), ]), 3)
  # Edge cell (1,3)
  expect_equal(sum(mask[idx(1, 3), ]), 5)
  # Interior cell (3,3)
  expect_equal(sum(mask[idx(3, 3), ]), 8)

  # Correct for 2x2 grid
  mask <- get_spatial_mask(2, 2)

  expected <- Matrix::sparseMatrix(
    i = c(1,1,1,2,2,2,3,3,3,4,4,4),
    j = c(2,3,4,1,3,4,1,2,4,1,2,3),
    x = rep(1, 12),
    dims = c(4,4)
  )

  expect_equal(mask, expected)

  # Subsetting by ids preserves adjacency structure
  ids = c(1,5,6,10,15)
  full_mask <- get_spatial_mask(5, 3)
  sub_mask  <- get_spatial_mask(5, 3, ids)

  expect_equal(dim(sub_mask), c(length(ids), length(ids)))
  expect_equal(sub_mask, full_mask[ids, ids])
})
