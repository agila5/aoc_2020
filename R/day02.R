# packages
library(stringr)

# Example - Part 1 --------------------------------------------------------
x <- c("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")
# 1 - Extract password criteria using a regex
# ^.*(?=\\:) is used to extract everything before ":" (excluded)
criterion <- str_extract(x, "^.*(?=\\:)")
# 2- Extract the letter, the minimum and the maximum
letter <- str_extract(criterion, "[a-z]")
# The following regex extracts the numbers before "-"
min_occurrences_letter <- as.integer(str_extract(criterion, "[0-9]+(?=-)"))
max_occurrences_letter <- as.integer(str_extract(criterion, "(?<=-)[0-9]+"))
# 3 - Extract the passwords
password <- str_extract(x, "(?<=\\: )\\w+")
# 4 - Count the occurrences of each letter
number_of_occurrences <- str_count(password, letter)
# 5 - Test conditions
sum(number_of_occurrences >= min_occurrences_letter & number_of_occurrences <= max_occurrences_letter)

# NB: I don't know how to use a regex like a{1, 3} since the letters might be non consecutive. 

# Solution - Part 1 -------------------------------------------------------
x <- readLines("data/input02")
criterion <- str_extract(x, "^.*(?=\\:)")
letter <- str_extract(criterion, "[a-z]")
min_occurrences_letter <- as.integer(str_extract(criterion, "[0-9]+(?=-)"))
max_occurrences_letter <- as.integer(str_extract(criterion, "(?<=-)[0-9]+"))
password <- str_extract(x, "(?<=\\: )\\w+")
number_of_occurrences <- str_count(password, letter)
sum(number_of_occurrences >= min_occurrences_letter & number_of_occurrences <= max_occurrences_letter)

rm(list = ls())

# Example - Part 2 --------------------------------------------------------
x <- c("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")
password <- str_extract(x, "(?<=\\: )\\w+")
criterion <- str_extract(x, "^.*(?=\\:)")
letter <- str_extract(criterion, "[a-z]")
first_position <- as.integer(str_extract(criterion, "[0-9]+(?=-)"))
second_position <- as.integer(str_extract(criterion, "(?<=-)[0-9]+"))
# I will test both conditions using a regex: 
first_condition_regex <- paste0("^.{", first_position - 1L, "}", letter)
second_condition_regex <- paste0("^.{", second_position - 1L, "}", letter)
first_condition <- str_detect(password, first_condition_regex)
second_condition <- str_detect(password, second_condition_regex)
# result
sum(xor(first_condition, second_condition))

# Solution - Part 2 -------------------------------------------------------
x <- readLines("data/input02")
password <- str_extract(x, "(?<=\\: )\\w+")
criterion <- str_extract(x, "^.*(?=\\:)")
letter <- str_extract(criterion, "[a-z]")
first_position <- as.integer(str_extract(criterion, "[0-9]+(?=-)"))
second_position <- as.integer(str_extract(criterion, "(?<=-)[0-9]+"))
first_condition_regex <- paste0("^.{", first_position - 1L, "}", letter)
second_condition_regex <- paste0("^.{", second_position - 1L, "}", letter)
first_condition <- str_detect(password, first_condition_regex)
second_condition <- str_detect(password, second_condition_regex)
sum(xor(first_condition, second_condition))
