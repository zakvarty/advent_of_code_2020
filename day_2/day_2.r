library(tidyverse)

input <- read.csv(
  file = "~/GitHub/advent_of_code_2020/day_2/input.txt",
  sep = " ",
  col.names = c("range","letter","password"),
  header = FALSE,
  stringsAsFactors = FALSE)

## Part 1
input %>%
  mutate(letter = substr(letter, 1,1),
         min = as.double(map_chr(str_split(input$range, pattern = "-"),1)),
         max = as.double(map_chr(str_split(input$range, pattern = "-"),2)),
         letter_count = map2_int(password, letter, str_count)) %>%
  filter(letter_count >= min, letter_count <= max) %>%
  NROW()

## Part 2
input %>%
  mutate(letter = substr(letter, 1,1),
         min = as.double(map_chr(str_split(input$range, pattern = "-"),1)),
         max = as.double(map_chr(str_split(input$range, pattern = "-"),2)),
         first_condition = substr(password, min, min) == letter,
         second_condition = substr(password, max, max) == letter,
         valid =  xor(first_condition, second_condition)) %>%
  filter(valid) %>%
  NROW()



