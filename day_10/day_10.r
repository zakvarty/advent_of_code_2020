## AOC 2020 - Day 1-: Adapter Array ------------------------------------------
library(tidyverse)
input <- as.integer(readLines("~/GitHub/advent_of_code_2020/day_10/input.txt"))
input <- as.integer(readLines("~/GitHub/advent_of_code_2020/day_10/test_input_1.txt"))

## Task 1: Using all adaptors, find the number of 1 and 3 jolt differences. What is their product?
answer <- input %>%
  sort() %>%                    # order adaptors increasing
  append(0,after = 0) %>%       # Add plug to start
  append(max(input) + 3) %>%    # Add the device to the end
  diff() %>%                    # Calculate differences in joltage
  table() %>%                   # There are only 1 and 3 jolt differences,
  prod()                        # so just take their product!



sorted_adaptors <- input %>%
  sort() %>%                    # order adaptors increasing
  append(0,after = 0) %>%       # Add plug to start
  append(max(input) + 3) %>%
  diff()

sorted_adaptor_jumps <- input %>%
  sort() %>%                    # order adaptors increasing
  append(0,after = 0) %>%       # Add plug to start
  append(max(input) + 3) %>%
  diff()

## Part 2: Find the number of valid adaptor combinations.

# Strategy:
  # To bridge an adaptor jump of 3, we have only one option.
  # need to work out the number of ways between each of those jumps.

# What is the max number of 1's in a row?
jump_string <- str_flatten(sorted_adaptor_jumps)
runs_of_ones <- table(str_length(str_split(jump_string,pattern = "3",simplify = TRUE)))
runs_of_ones

## COUNTING COMBINATIONS: MY REASONING.
## (Read: 0>3>4>6 as: 0j plug then 3j adpator then 4j adaptor then 6j device)
## 33 = 1 option (e.g {0,3,7} only has 3>7)
## 313 = 1 (e.g. {0,3,4,7} only has 3>4>7)
## 3113 = 2 options (e.g. {0,3,4,5,8} has 3>5>8 or 3>4>5>8)
## 31113 = 4 options (e.g. {0,3,4,5,6,9} has 3>6>9, 3>5>6>9, 3>4>6>9, 3>4>5>6>9)
## 311113 = 7 options (e.g. {0,3,4,5,6,7,10} has 3>6>7>10, 3>5>7>10, 3>4>7>10,
##                          3>4>5>7>10, 3>4>6>7>10, 3>5>6>7>10, 3>4>5>6>7>10)
n_options <- c(1,1,2,4,7)

#total number of combinations
ans <- prod(n_options^as.integer(runs_of_ones))
options(scipen=999)
ans

