# Example - Part 1 --------------------------------------------------------
x <- c(1721, 979, 366, 299, 675, 1456)
outer_product <- outer(x, x, "+")
which(outer_product == 2020, arr.ind = TRUE)
# check 
sum(x[c(1, 4)])
x[1] * x[4]

# Solution - Part 1 -------------------------------------------------------
x <- as.numeric(readLines("data/input01"))
outer_product <- outer(x, x, "+")
which(outer_product == 2020, arr.ind = TRUE)
# check 
sum(x[c(162, 143)])
x[162] * x[143]

# Example - Part 2 --------------------------------------------------------
x <- c(1721, 979, 366, 299, 675, 1456)
outer_product <- outer(outer(x, x, "+"), x, "+")
which(outer_product == 2020, arr.ind = TRUE)
# check
sum(x[c(2, 3, 5)])
x[2] * x[3] * x[5]

# Solution - Part 2 -------------------------------------------------------
x <- as.numeric(readLines("data/input01"))
outer_product <- outer(outer(x, x, "+"), x, "+")
which(outer_product == 2020, arr.ind = TRUE)
# check
sum(x[c(166, 109, 46)])
x[46] * x[109] * x[166]
