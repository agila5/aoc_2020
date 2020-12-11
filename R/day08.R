# Example - Part 1 --------------------------------------------------------
input <- c(
  "nop +0", "acc +1", "jmp +4", "acc +3", "jmp -3", "acc -99", "acc +1", 
  "jmp -4", "acc +6"
)
# Start from the first operation
i <- 1
total <- 0
# Logical vector to see when we run into a loop
run <- logical(length(input))

while (TRUE) {
  # Check if the code associated to the ith operation was already executed
  if (run[[i]]) break()
  
  # Extract the string associated to the ith operation and changed the run flag
  # to TRUE
  operation_string <- input[[i]]
  run[[i]] <- TRUE
  
  # Extracth the code of the operation
  operation <- substr(operation_string, 1, 3)
  
  # The following behaviour depends on the value of operation. If operation ==
  # "jmp", I need to jump to a new instruction
  if (operation == "jmp") {
    argument <- readr::parse_number(operation_string)
    i <- i + argument
    next()
  }
  
  # If operation == "acc", then I need to increase or decrease the value of
  # total according to the parameter associated to the operation
  if (operation == "acc") {
    argument <- readr::parse_number(operation_string)
    total <-  total + argument
  }
  
  # If operation = "nop", then I don't need to do anything
  i <- i + 1
}

# Solution - Part 1 -------------------------------------------------------
input <- readLines("data/input08")
i <- 1
total <- 0
run <- logical(length(input))

while (TRUE) {
  if (run[[i]]) break()
  operation_string <- input[[i]]
  run[[i]] <- TRUE
  operation <- substr(operation_string, 1, 3)
  if (operation == "jmp") {
    argument <- readr::parse_number(operation_string)
    i <- i + argument
    next()
  }
  if (operation == "acc") {
    argument <- readr::parse_number(operation_string)
    total <-  total + argument
  }
  i <- i + 1
}

print(total)
rm(list = ls())

# Example - Part 2 --------------------------------------------------------
input <- c(
  "nop +0", "acc +1", "jmp +4", "acc +3", "jmp -3", "acc -99", "acc +1", 
  "jmp -4", "acc +6"
)

# Find the location of all nop or jmp
nop_or_jmp <- which(grepl("(nop|jmp)", input))

for (j in nop_or_jmp) {
  # Modify the jth operation
  substr(input[[j]], 1, 3) <- ifelse(substr(input[[j]], 1, 3) == "nop", "jmp", "nop")
  
  # Run the code
  i <- 1
  total <- 0
  run <- logical(length(input))
  
  while (TRUE) {
    if (i > length(input)) {
      message("Stop! Total is equal to ", total)
      break()
    }
    if (run[[i]]) break()
    operation_string <- input[[i]]
    run[[i]] <- TRUE
    operation <- substr(operation_string, 1, 3)
    if (operation == "jmp") {
      argument <- readr::parse_number(operation_string)
      i <- i + argument
      next()
    }
    if (operation == "acc") {
      argument <- readr::parse_number(operation_string)
      total <-  total + argument
    }
    i <- i + 1
  }
  
  # Check if the previous code run succesfully
  if (i > length(input)) break()
  
  # Revert the first operation
  substr(input[[j]], 1, 3) <- ifelse(substr(input[[j]], 1, 3) == "nop", "jmp", "nop")
}

# Solution - Part 2 --------------------------------------------------------
input <- readLines("data/input08")
nop_or_jmp <- which(grepl("(nop|jmp)", input))

for (j in nop_or_jmp) {
  substr(input[[j]], 1, 3) <- ifelse(substr(input[[j]], 1, 3) == "nop", "jmp", "nop")
  i <- 1
  total <- 0
  run <- logical(length(input))
  while (TRUE) {
    if (i > length(input)) {
      message("Stop! Total is equal to ", total)
      break()
    }
    if (run[[i]]) break()
    operation_string <- input[[i]]
    run[[i]] <- TRUE
    operation <- substr(operation_string, 1, 3)
    if (operation == "jmp") {
      argument <- readr::parse_number(operation_string)
      i <- i + argument
      next()
    }
    if (operation == "acc") {
      argument <- readr::parse_number(operation_string)
      total <-  total + argument
    }
    i <- i + 1
  }
  if (i > length(input)) break()
  substr(input[[j]], 1, 3) <- ifelse(substr(input[[j]], 1, 3) == "nop", "jmp", "nop")
}
