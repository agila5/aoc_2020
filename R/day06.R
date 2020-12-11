# Example - Part 1 --------------------------------------------------------
input <- readLines("data/input06-test")
# Find the blank lines
blank_lines <- input == ""
# Find a unique ID for each group
group_id <- cumsum(blank_lines)[-which(blank_lines)]
# Exclude blank lines
input_clean <- input[-which(blank_lines)]
# group input data
input_list <- split(input_clean, group_id)
# Define a function to count the number of answers in each group
my_count_answers <- function(data) {
  splitted_data <- unlist(strsplit(data, ""))
  
  length(unique(splitted_data))
}
# result
sum(vapply(input_list, my_count_answers, integer(1)))

# Solution - Part 1 -------------------------------------------------------
input <- readLines("data/input06")
blank_lines <- input == ""
group_id <- cumsum(blank_lines)[-which(blank_lines)]
input_clean <- input[-which(blank_lines)]
input_list <- split(input_clean, group_id)
sum(vapply(input_list, my_count_answers, integer(1)))

rm(list = ls())
    
# Example - Part 2 --------------------------------------------------------
input <- readLines("data/input06-test")
# Find the blank lines
blank_lines <- input == ""
# Find a unique ID for each group
group_id <- cumsum(blank_lines)[-which(blank_lines)]
# Exclude blank lines
input_clean <- input[-which(blank_lines)]
# group input data
input_list <- split(input_clean, group_id)
# Define a function to count the common answers in each group
my_count_commond_answers <- function(data) {
  # Define the unique answers in each group
  splitted_data <- strsplit(data, "")
  # The answers are a-z so I can use factor + table to define a "commond"
  # frequency table
  splitted_data_as_factors <- lapply(splitted_data, factor, levels = letters)
  splitted_data_as_table <- do.call("rbind", lapply(splitted_data_as_factors, table))
  # Count how many letters are shared between all responses
  sum(colMeans(splitted_data_as_table) == 1)
}

vapply(input_list, my_count_commond_answers, integer(1))

# Solution - Part 2 --------------------------------------------------------
input <- readLines("data/input06")
blank_lines <- input == ""
group_id <- cumsum(blank_lines)[-which(blank_lines)]
input_clean <- input[-which(blank_lines)]
input_list <- split(input_clean, group_id)

sum(vapply(input_list, my_count_commond_answers, integer(1)))

