#' @title Reorder dataset columns by variable or location grouping
#'
#' @description Reorganizes columns of a dataset that follows a "variable.location"
#' naming convention (e.g. "tas.1", "tas.2", "pr.1" etc.). The function allows
#' switching between variable-major and location-major layouts.
#'
#' Two ordering strategies are supported:
#'   - variable-major : all locations for each variable are grouped together
#'   - location-major : all variables for each location are grouped together
#
#'
#' @param data Data frame with columns named as "variable.location", e.g. "tas.1" or "tas.loc1"
#' @param direction Reordering strategy: "variable-major" or "location-major"
#' @param order Optional: Integer vector specifying a custom variable order
#'
#' @returns Data frame with columns reordered according to the selected strategy
#' @export
#'
reorder_dataset <- function(data,
                            direction = c("variable-major", "location-major"),
                            order = NULL) {

  # ---------------------------------------------------------------------------
  # Step 1: Extract unique variable names (text before the dot)
  # ---------------------------------------------------------------------------
  var_names <- unique(sub("\\..*$", "", colnames(data)))

  # Apply custom variable ordering if provided
  if (!is.null(order)) {
    var_names <- var_names[order]
  }

  # ---------------------------------------------------------------------------
  # Step 2: Extract location identifiers (text after the dot)
  # ---------------------------------------------------------------------------
  loc_numbers <- unique(sub("^.*\\.", "", colnames(data)))

  # ---------------------------------------------------------------------------
  # Step 3: Construct new column order
  # ---------------------------------------------------------------------------
  if (direction == "variable-major") {

    # Group columns by variable:
    # var1.loc1, var1.loc2, ..., var2.loc1, var2.loc2, ...
    new_order <- as.vector(
      sapply(var_names, function(v) paste0(v, ".", loc_numbers))
    )

  } else if (direction == "location-major") {

    # Group columns by location:
    # var1.loc1, var2.loc1, ..., var1.loc2, var2.loc2, ...
    new_order <- as.vector(
      sapply(loc_numbers, function(i) paste0(var_names, ".", i))
    )

  } else {
    stop("Please select either 'variable-major' or 'location-major' ordering.")
  }

  # ---------------------------------------------------------------------------
  # Step 4: Reorder and return dataset
  # ---------------------------------------------------------------------------
  return(data.frame(data)[, new_order])
}
