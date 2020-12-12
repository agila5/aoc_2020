# Example - Part 1 ------------------------------------------------------------
input <- c(
  35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 
  277, 309, 576
)
# Define the ID of the buffer (which will be incremented by 1)
preamble <- 1:5
# and the number that should be test (again, will be incremented by 1)
test <- 6L
while (TRUE) {
  if (test > length(input)) {
    message("Something's wrong :(")
    break
  }
  
  all_combinations <- outer(input[preamble], input[preamble], "+")
  # I need to consider only "distinct" combinations
  diag(all_combinations) <- NA_real_
  
  if (!input[test] %in% all_combinations) {
    message(
      "The first number that does not follow this rule is ", 
      input[test], "."
    )
    break
  }
  
  # Increment ID of preamble and test
  preamble <- preamble + 1L
  test <- test + 1L
}

# Solution - Part 1 -------------------------------------------------------
input <- as.numeric(readLines("data/input09"))
preamble <- 1:25
test <- 26L
while (TRUE) {
  if (test > length(input)) {
    message("Something's wrong :(")
    break
  }
  
  all_combinations <- outer(input[preamble], input[preamble], "+")
  diag(all_combinations) <- NA_real_
  
  if (!input[test] %in% all_combinations) {
    message(
      "The first number that does not follow this rule is ", 
      input[test], "."
    )
    break
  }
  
  preamble <- preamble + 1L
  test <- test + 1L
}

rm(list = ls())

# Example - Part 2 ------------------------------------------------------------
input <- c(
  35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 
  277, 309, 576
)
# The number that does not follow the rule is 
offending_number <- 127
# Exclude all numbers bigger than offending_numbers
input <- input[input < offending_number]

# Find the sum of all contiguous groups of three, four, five, ... numbers
i <- 3
while (TRUE) {
  if (i > length(input)) {
    message("Something's wrong :(")
    break
  }
  
  # Estimate all rolling sums
  all_rolling_sums <- data.table::frollsum(input, i, algo = "exact")
  all_rolling_sums <- na.omit(all_rolling_sums)
  
  if (offending_number %in% all_rolling_sums) {
    numbers <- input[seq(which(all_rolling_sums == offending_number), length.out = i)]
    
    message(
      "The offending number, i.e. ", offending_number, ", can be written as ", 
      "the sum of ", i, " numbers! These numbers are ", 
      paste(numbers, collapse = " "), ". ", 
      "The smallest one is ", min(numbers), ", the largest one is ", 
      max(numbers), ", and their sum is ", 
      min(numbers) + max(numbers), "."
    )
    break()
  }
  
  i <- i + 1
}

# Solution - Part 2 -------------------------------------------------------
input <- as.numeric(readLines("data/input09"))
offending_number <- 2089807806
input <- input[input < offending_number]
i <- 3
while (TRUE) {
  if (i > length(input)) {
    message("Something's wrong :(")
    break
  }
  
  # Estimate all rolling sums
  all_rolling_sums <- data.table::frollsum(input, i, algo = "exact")
  all_rolling_sums <- na.omit(all_rolling_sums)
  
  if (offending_number %in% all_rolling_sums) {
    numbers <- input[seq(which(all_rolling_sums == offending_number), length.out = i)]
    
    message(
      "The offending number, i.e. ", offending_number, ", can be written as ", 
      "the sum of ", i, " numbers! These numbers are ", 
      paste(numbers, collapse = " "), ". ", 
      "The smallest one is ", min(numbers), ", the largest one is ", 
      max(numbers), ", and their sum is ", 
      min(numbers) + max(numbers), "."
    )
    break()
  }
  
  i <- i + 1
}
