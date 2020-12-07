# Advent of code Day 7: Handy Haversacks----

## Part 0: read data -----
library(tidyverse)
input <- readLines("./input.txt")

### (REGEX based on solution by @Emil_Hvitfeldt and using https://regex101.com/)
bags <- str_extract_all(string = input, pattern = "(?<=[0-9] ).*?(?= bag)")
number <- str_extract_all(string = input, pattern = "[0-9](?= .*? bag)") %>%
  map(as.integer)

names(bags) <- str_extract_all(string = input, pattern = "(?<=\n|^).*?(?= bags)")
names(number) <- names(bags)

## Part one: How many bags that contain at least one gold bag? ----
contains_shiny_gold <- function(bag_colour, bags_obj){
  sub_bags <- bags_obj[[bag_colour]]
  n_sub_bags <- length(sub_bags)
  if (n_sub_bags == 0){
    return(FALSE)
  } else if ("shiny gold" %in% sub_bags){
    return(TRUE)
  } else {
    # Check if one or more sub_bags contain "shiny gold"
    sub_bag_results = rep(FALSE, n_sub_bags)
    sub_bag_index <- 1
    while ((sub_bag_index <= n_sub_bags) & (sum(sub_bag_results) == 0)){
      sub_bag_results[sub_bag_index] <- contains_shiny_gold(sub_bags[sub_bag_index], bags_obj)
      sub_bag_index <- sub_bag_index + 1
    }
    return(any(sub_bag_results))
  }
}

# apply to each bag and add the results
map_lgl(names(bags), contains_shiny_gold, bags_obj = bags) %>%
  sum()

## Part two: How many bags are nested in a shiny gold bag? ----
n_sub_bags <- function(bag_colour, bags_obj, num_obj){
  sub_bag_count <- 0

  sub_bags <- bags_obj[[bag_colour]]
  multiplicity <- num_obj[[bag_colour]]

  for(i in seq_along(sub_bags)){
    m = multiplicity[i]
    n = n_sub_bags(sub_bags[i], bags_obj, num_obj)
    sub_bag_count <- sub_bag_count + m * (n + 1)
  }

  return(sub_bag_count)
}

n_sub_bags("shiny gold", bags, number)

