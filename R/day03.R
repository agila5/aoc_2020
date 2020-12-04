# Example - Part 1 --------------------------------------------------------
x <- readLines("data/input03-test")
# Convert into matrix format (since I will use matrix subsetting)
x_mat <- do.call(rbind, strsplit(x, ""))
# Determine how many times I have to copy the matrix. The number of copies is
# given by the integer division of (1 + 3 * (nrow(mat) - 1)) and ncol(mat).  The
# first 1 is determined by the starting column, 3 is the column step and I need
# to subtract 1 since I don't need to go down from the last column. The integer
# division determines how many times I have to replicate the x_mat. For example,
# if nrow(mat) = 11 and ncol(mat) = 11, then 1 + 3 * (nrow(mat) - 1) is 31, 31
# %/% 11 is equal to 2 (since 11 * 3 = 33), so, at the end, I need (at least) 2
# extra copies.
extra_copies <- (1 + 3 * (nrow(x_mat) - 1)) %/% ncol(x_mat)
x_mat_replicates <- do.call(
  "cbind", 
  replicate(
    # Add 1 since I need to replicate the original matrix 1 more.
    extra_copies + 1, 
    x_mat, 
    simplify = FALSE
  )
)
# The row/col indices of the locations that I will check are given by:
row_indices <- seq(2, nrow(x_mat_replicates))
col_indices <- seq(4, by = 3, length.out = nrow(x_mat_replicates) - 1)
# I need to build a matrix for matrix subsetting
mat_indices <- cbind(row_indices, col_indices)
# Subset
locations <- x_mat_replicates[mat_indices]
# Count occurrences
sum(locations == "#")

# Solution - Part 1 -------------------------------------------------------
x <- readLines("data/input03")
x_mat <- do.call(rbind, strsplit(x, ""))
extra_copies <- (1L + 3L * (nrow(x_mat) - 1L)) %/% ncol(x_mat)
x_mat_replicates <- do.call(
  "cbind", 
  replicate(
    extra_copies + 1L, 
    x_mat, 
    simplify = FALSE
  )
)
row_indices <- seq(2, nrow(x_mat_replicates))
col_indices <- seq(4, by = 3, length.out = nrow(x_mat_replicates) - 1)
mat_indices <- cbind(row_indices, col_indices)
locations <- x_mat_replicates[mat_indices]
sum(locations == "#")

rm(list = ls())

# Example - Part 2 --------------------------------------------------------
x <- readLines("data/input03-test")
x_mat <- do.call(rbind, strsplit(x, ""))
# I need to generalize the previous approach for generals row steps and column
# steps
my_count_trees <- function(x_mat, col_step, row_step) {
  extra_copies <- (1 + col_step * (nrow(x_mat) - 1)) %/% (ncol(x_mat) * row_step)
  x_mat_replicates <- do.call(
    "cbind", 
    replicate(
      extra_copies + 1, 
      x_mat, 
      simplify = FALSE
    )
  )
  
  # The row/col indices of the locations that I will check are given by:
  row_indices <- seq(1 + row_step, nrow(x_mat_replicates), by = row_step)
  col_indices <- seq(1 + col_step, by = col_step, length.out = length(row_indices))
  mat_indices <- cbind(row_indices, col_indices)
  locations <- x_mat_replicates[mat_indices]
  sum(locations == "#")
}
my_count_trees(x_mat, 1, 1)
my_count_trees(x_mat, 3, 1)
my_count_trees(x_mat, 5, 1)
my_count_trees(x_mat, 7, 1)
my_count_trees(x_mat, 1, 2)

# Solution - Part 2 -------------------------------------------------------
x <- readLines("data/input03")
x_mat <- do.call(rbind, strsplit(x, ""))
my_count_trees(x_mat, 1, 1)
my_count_trees(x_mat, 3, 1)
my_count_trees(x_mat, 5, 1)
my_count_trees(x_mat, 7, 1)
my_count_trees(x_mat, 1, 2)
