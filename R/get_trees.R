#' @title Create a spatial adjacency mask for a regular grid
#' @description Constructs a sparse adjacency matrix representing spatial neighborhood
#' relationships on a 2D grid. Each grid cell is connected to its immediate
#' neighbors: horizontal, vertical, and diagonal (8-neighborhood).
#'
#' The resulting matrix can be used as a spatial constraint mask, e.g.
#' to restrict allowed edges in a spatial R-vine or graphical model.
#' @param rows Number of rows in the grid
#' @param cols Number of columns in the grid
#' @param ids Optional vector of cell indices to subset the grid
#' (useful when working with masked or irregular domains)
#'
#' @returns Sparse adjacency matrix of dimension (`n_cells x n_cells`),
#' where entry `(i, j) = 1` if cells `i` and `j` are neighbors
#' @details
#' The grid cells are numbered column-wise, i.e., it starts with the grid cell
#' in the bottom-left corner (row 1, column 1) as cell 1, then moves up the first
#' column, then continues at the bottom cell of the second column, and so on.
#'
#' @export
#'
#' @examples # 5x5 grid with full neighborhood structure
#' mask <- get_spatial_mask(5, 5)
#'
#' # Subset to selected grid cells only
#' mask_sub <- get_spatial_mask(5, 5, ids = c(1, 2, 6, 7))
get_spatial_mask <- function(rows, cols, ids = NULL) {

  # Total number of grid cells
  n_cells <- rows * cols

  # Initialize vectors to store adjacency pairs (i, j)
  # These will later define the nonzero entries of the sparse matrix
  i_indices <- integer(0)
  j_indices <- integer(0)

  # Loop over grid coordinates
  for (r in 1:cols) {
    for (c in 1:rows) {

      # Convert 2D grid coordinates (r, c) to 1D index
      idx <- (r - 1) * rows + c

      # --- Horizontal neighbor (right) ---
      if (c < rows) {
        i_indices <- c(i_indices, idx, idx + 1)
        j_indices <- c(j_indices, idx + 1, idx)
      }

      # --- Vertical neighbor (down) ---
      if (r < cols) {
        i_indices <- c(i_indices, idx, idx + rows)
        j_indices <- c(j_indices, idx + rows, idx)
      }

      # --- Diagonal neighbor (down-right) ---
      if (r < cols && c < rows) {
        i_indices <- c(i_indices, idx, idx + rows + 1)
        j_indices <- c(j_indices, idx + rows + 1, idx)
      }

      # --- Diagonal neighbor (down-left) ---
      if (r < cols && c > 1) {
        i_indices <- c(i_indices, idx, idx + rows - 1)
        j_indices <- c(j_indices, idx + rows - 1, idx)
      }
    }
  }

  # Build sparse adjacency matrix from (i, j) index pairs
  # Each neighbor relationship is encoded with value 1
  adj_matrix <- Matrix::sparseMatrix(
    i = i_indices,
    j = j_indices,
    x = 1,
    dims = c(n_cells, n_cells),
    symmetric = FALSE
  )

  # Optionally restrict the adjacency matrix to a subset of cells
  if (!is.null(ids)) {
    adj_matrix <- adj_matrix[ids, ids]
  }

  return(adj_matrix)
}
