# Example - Part 1 --------------------------------------------------------
x <- readLines("data/input04-test-a")
# Find the blank lines
blank_lines <- x == ""
# Find a unique ID for the fields in each passport
passport_id <- cumsum(blank_lines)[-which(blank_lines)]
# Exclude blank lines
passport_data <- x[-which(blank_lines)]
# Group passport data
passport_data_grouped <- split(passport_data, passport_id)
# Collapse multiple fields into a unique row
passport_data_grouped <- lapply(passport_data_grouped, paste, collapse = " ")

# Write all the tests (this is far from the ideal approach)
byr_field <- vapply(passport_data_grouped, grepl, logical(1), pattern = "byr\\:", perl = TRUE)
iyr_filed <- vapply(passport_data_grouped, grepl, logical(1), pattern = "iyr\\:", perl = TRUE)
eyr_field <- vapply(passport_data_grouped, grepl, logical(1), pattern = "eyr\\:", perl = TRUE)
hgt_field <- vapply(passport_data_grouped, grepl, logical(1), pattern = "hgt\\:", perl = TRUE)
hcl_field <- vapply(passport_data_grouped, grepl, logical(1), pattern = "hcl\\:", perl = TRUE)
ecl_field <- vapply(passport_data_grouped, grepl, logical(1), pattern = "ecl\\:", perl = TRUE)
pid_field <- vapply(passport_data_grouped, grepl, logical(1), pattern = "pid\\:", perl = TRUE)

# bind results
checks <- rbind(byr_field, iyr_filed, eyr_field, hgt_field, hcl_field, ecl_field, pid_field)

# count valid passports
sum(colSums(checks) == 7)

# Solution - Part 1 -------------------------------------------------------
x <- readLines("data/input04")
blank_lines <- x == ""
passport_id <- cumsum(blank_lines)[-which(blank_lines)]
passport_data <- x[-which(blank_lines)]
passport_data_grouped <- split(passport_data, passport_id)
passport_data_grouped <- lapply(passport_data_grouped, paste, collapse = " ")

byr_field <- vapply(passport_data_grouped, grepl, logical(1), pattern = "byr\\:", perl = TRUE)
iyr_filed <- vapply(passport_data_grouped, grepl, logical(1), pattern = "iyr\\:", perl = TRUE)
eyr_field <- vapply(passport_data_grouped, grepl, logical(1), pattern = "eyr\\:", perl = TRUE)
hgt_field <- vapply(passport_data_grouped, grepl, logical(1), pattern = "hgt\\:", perl = TRUE)
hcl_field <- vapply(passport_data_grouped, grepl, logical(1), pattern = "hcl\\:", perl = TRUE)
ecl_field <- vapply(passport_data_grouped, grepl, logical(1), pattern = "ecl\\:", perl = TRUE)
pid_field <- vapply(passport_data_grouped, grepl, logical(1), pattern = "pid\\:", perl = TRUE)

checks <- rbind(byr_field, iyr_filed, eyr_field, hgt_field, hcl_field, ecl_field, pid_field)
sum(colSums(checks) == 7)

rm(list = ls())

# Example - Part 2 --------------------------------------------------------
test_passports <- function(path) {
  # read-in and format data
  x <- readLines(path)
  blank_lines <- x == ""
  passport_id <- cumsum(blank_lines)[-which(blank_lines)]
  passport_data <- x[-which(blank_lines)]
  passport_data_grouped <- split(passport_data, passport_id)
  passport_data_vector <- vapply(passport_data_grouped, paste, character(1), collapse = " ")
  
  # Test presence of fields and conditions
  # byr
  passport_data_vector <- grep("byr\\:", passport_data_vector, perl = TRUE, value = TRUE)
  test_4_digits_byr <- grepl("byr\\:\\d{4}", passport_data_vector, perl = TRUE)
  passport_data_vector <- passport_data_vector[test_4_digits_byr]
  extract_year <- as.numeric(stringr::str_extract(passport_data_vector, "(?<=byr\\:)\\d{4}"))
  test_year_range <- extract_year >= 1920 & extract_year <= 2002
  passport_data_vector <- passport_data_vector[test_year_range]
  
  # iyr
  passport_data_vector <- grep("iyr\\:", passport_data_vector, perl = TRUE, value = TRUE)
  test_4_digits_iyr <- grepl("iyr\\:\\d{4}", passport_data_vector, perl = TRUE)
  passport_data_vector <- passport_data_vector[test_4_digits_iyr]
  extract_digits <- as.numeric(stringr::str_extract(passport_data_vector, "(?<=iyr\\:)\\d{4}"))
  test_digits_range <- extract_digits >= 2010 & extract_digits <= 2020
  passport_data_vector <- passport_data_vector[test_digits_range]
  
  # eyr
  passport_data_vector <- grep("eyr\\:", passport_data_vector, perl = TRUE, value = TRUE)
  test_4_digits_eyr <- grepl("eyr\\:\\d{4}", passport_data_vector, perl = TRUE)
  passport_data_vector <- passport_data_vector[test_4_digits_eyr]
  extract_digits <- as.numeric(stringr::str_extract(passport_data_vector, "(?<=eyr\\:)\\d{4}"))
  test_digits_range <- extract_digits >= 2020 & extract_digits <= 2030
  passport_data_vector <- passport_data_vector[test_digits_range]
  
  # hgt
  passport_data_vector <- grep("hgt\\:", passport_data_vector, perl = TRUE, value = TRUE)
  test_cm_in <- grepl("hgt\\:\\d*(cm|in)", passport_data_vector, perl = TRUE)
  passport_data_vector <- passport_data_vector[test_cm_in]
  extract_cm <- as.numeric(stringr::str_extract(passport_data_vector, "(?<=hgt\\:)\\d+(?=cm)"))
  test_cm <- is.na(extract_cm) | (extract_cm >= 150 & extract_cm <= 193)
  passport_data_vector <- passport_data_vector[test_cm]
  extract_in <- as.numeric(stringr::str_extract(passport_data_vector, "(?<=hgt\\:)\\d+(?=in)"))
  test_in <- is.na(extract_in) | (extract_in >= 59 & extract_in <= 76)
  passport_data_vector <- passport_data_vector[test_in]
  
  # hcl
  passport_data_vector <- grep("hcl\\:", passport_data_vector, perl = TRUE, value = TRUE)
  test_6_characters <- grepl("hcl\\:\\#[0-9a-f]{6}", passport_data_vector, perl = TRUE)
  passport_data_vector <- passport_data_vector[test_6_characters]
  
  # ecl 
  passport_data_vector <- grep("ecl\\:", passport_data_vector, perl = TRUE, value = TRUE)
  test_color <- grepl("ecl\\:(amb|blu|brn|gry|grn|hzl|oth)", passport_data_vector, perl = TRUE)
  passport_data_vector <- passport_data_vector[test_color]
  
  # pid 
  passport_data_vector <- grep("pid\\:", passport_data_vector, perl = TRUE, value = TRUE)
  test_9_digits <- nchar(stringr::str_extract(passport_data_vector, "(?<=pid\\:)\\d+")) == 9
  passport_data_vector <- passport_data_vector[!is.na(test_9_digits) & test_9_digits]

  # return result
  length(passport_data_vector)
}

test_passports("data/input04-test-a")
test_passports("data/input04-test-b")
test_passports("data/input04-test-c")
test_passports("data/input04")
