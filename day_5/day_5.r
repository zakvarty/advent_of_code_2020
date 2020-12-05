#  --- Day 5: Binary Boarding ---

## Load data and packages
library(tidyverse)
binary_seats <- read_csv("./GitHub/advent_of_code_2020/day_5/input.txt",col_names = "string")

## Function to convert binary seat search string to seat ID (vectorised)
binary_to_id <- function(strings){
  row <- column <- rep(0, length(strings))
  for(i in 1:7){
      row <- row + (substr(strings, i, i) == "B") * 2^(7- i)
  }
  for(i in 8:10){
      column <- column + (substr(strings, i, i) == "R") * 2^(10-i)
  }
  id <- 8 * row + column
  return(id)
}

## Test function on given examples
binary_to_id(c("FBFBBFRLR","BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"))


## Task 1: What is the highest seat id?
binary_to_id(binary_seats$string) %>%
  max()


## Taks 2: Find your seat.
# (Seat IDs 0 to x do not exist on this plane, nor do y to 1023. A
# All other seats are full.)

is_taken <- 0:1023 %in% binary_to_id(binary_seats$string)
first_taken <- which.max(is_taken)
my_seat <- which.min(is_taken[-(1:first_taken)]) + first_taken
# revert to 0-indexing
my_seat - 1
