## AOC 2020 - Day 9: Encoding Error ------------------------------------------

## Part 0: Read and format inputs ----------------------------------------------
library(tidyverse)
input_path <- "~/Github/advent_of_code_2020/day_9/"
example_input <- as.double(readLines(paste0(input_path,"example_input.txt")))
input <- as.double(readLines(paste0(input_path,"input.txt")))


## Part 1: A number is valid if if can be expressed as the sum of two ----------
## of the previous preamble_length numbers. Find the value of the first --------
## number that is not valid. ---------------------------------------------------

check_is_valid <- function(index, num_vec, preamble_length){
  if(index <= preamble_length) return(TRUE)

  desired_total <- num_vec[index]
  summands <- num_vec[(index - preamble_length ): (index - 1)]

  pair_sums <- outer(X = summands, Y = summands, FUN = "+")
  pair_sums <- pair_sums * upper.tri(pair_sums, diag = FALSE)

  found_pair <- desired_total %in% pair_sums
  return(found_pair)
}


is_valid <- seq_along(input) %>%
  map_lgl( .f = check_is_valid, num_vec = input, preamble_length = 25)

answer_1 <- input[!is_valid]

## Part 2: Find the contiguous set of numbers that sum to the previous ---------
## answer. What is the sum of the largest and smallest numbers in this set? ----

locate_window <- function(num_vec, target_sum){
  N <- length(num_vec)
  for (start_index in 1:(N - 1)) {
    for (end_index in (start_index + 1):N) {
      sums_to_target <- sum(input[start_index:end_index]) == target
      if (sums_to_target){ return(c(start_index, end_index))
      }
    }
  }
}

window_edges <- locate_window(input, answer_1)
window <- input[window_edges[1]:window_edges[2]]
answer_2 <- min(window) + max(window)
