# AOC 2020 Day 6: Custom Customs -----

## Part 0: Reading input
library(tidyverse)
input <- read_lines("~/GitHub/advent_of_code_2020/day_6/input.txt")

## Part 1: Sum unique letter count per group over groups

### concatonate string vectors
str_collect <- function(str_vec){
  group_count <- sum(str_vec == "") + 1
  out <- rep("", group_count)
  group <- 1

  for(string in str_vec){
    if(string != ""){
      out[group] <- paste0(out[group], string)
    } else {
      group <- group + 1
    }
  }
  return(out)
}

### ANSWER
str_collect(input) %>%                  # formatted input
map(str_detect, pattern = letters) %>%  # are "a"-"z" present in each group?
map_dbl(sum) %>%                        # unique letter count per group
sum()                                   # total across all groups


## Part 2:
## For each group, count the number of questions to which everyone
## answered "yes". What is the sum of those counts?

### Format input to be list of vector of strings
str_group <- function(str_vec){
  group_count <- sum(str_vec == "") + 1
  group_list <- vector(mode = "list", length = group_count)

  group <- 1
  group_vec <- c()

  for(string in c(str_vec, "")){
    if(string != ""){
      group_vec <- c(group_vec, string)
    } else {
      group_list[[group]] <- group_vec
      group <- group + 1
      group_vec <- c()
    }
  }
  return(group_list)
}

### Check which of a-z are in a string.
### Takes a single string
### Returns logical(26): Are a-z in the string?
letters_in_string <- function(str){
  str_detect(string =  str, pattern = letters)
}

### Takes vector of strings.
### Returns logical(26): are each of a-z in all strings?
letters_in_all_strings <- function(str_vec){
Reduce("*", map(str_vec, letters_in_string))
}

### ANSWER
input %>%
  str_group() %>%                    # Format input
  map(letters_in_all_strings) %>%    # For each group which letters shared?
  map_dbl(sum) %>%                   # How many letters shared per group?
  sum()                              # Sum for required answer.


