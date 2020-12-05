# Example - Part 1 --------------------------------------------------------
x <- c("FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL")
x_mat <- do.call("rbind", strsplit(x, ""))
# Define a function that will be applied to each row
find_seat_id <- function(row_and_place) {
  # 1a. Extract row
  row <- row_and_place[1:7]
  # 1b. Convert row to binary
  row <- ifelse(row == "F", 0, 1)
  # 1c. Convert binary to decimal
  row_decimal <- strtoi(paste0(row, collapse = ""), base = 2L)
  
  # 2a. Extract seat
  seat <- row_and_place[8:10]
  # 2b. Convert seat to binary
  seat <- ifelse(seat == "R", 1, 0)
  # 2c. Convert seat to decimal
  seat_decimal <- strtoi(paste0(seat, collapse = ""), base = 2L)
  
  # Unique ID
  row_decimal * 8 + seat_decimal
}
apply(x_mat, 1, find_seat_id)        

# Solution - Part 1 -------------------------------------------------------
x <- readLines("data/input05")
x_mat <- do.call("rbind", strsplit(x, ""))
ids <- apply(x_mat, 1, find_seat_id)   
max(ids)

# Solution - Part 2 -------------------------------------------------------
rows <- 0:127
seats <- 0:7
all_ids <- outer(rows, seats, FUN = function(x, y) 8 * x + y)
matrix(all_ids %in% ids, nrow = 128, ncol = 8)
82 * 8 + 5
