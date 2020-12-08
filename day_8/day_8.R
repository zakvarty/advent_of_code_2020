## AOC 2020 - Day 8: Handheld Halting ------------------------------------------

# Part 0: load data and packages -----------------------------------------------

# Packages
library(tidyverse)

# Data
example_input <- readLines(paste0(here::here(),"/day_8/test_input.txt"))
input <- readLines(paste0(here::here(),"/day_8/input.txt"))


# Part 1: Find accumulator value at start of infinite loop ---------------------

status_at_inf_loop <- function(input_data){
  # Fomat input strings into usable data frame
  input_tibble <- tibble(input_string = input_data) %>%
    separate(input_string, into = c("command", "number"), sep = " ") %>%
    mutate(number = as.integer(number))

  accumulator <- 0
  current_line <- 1
  previous_lines <- c()

  # Follow flow control until you loop beck to a previous step
  while (!(current_line %in% previous_lines)){
    current_move <- input_tibble$command[current_line]
    previous_lines <- c(previous_lines, current_line)
    if(current_move == "nop"){
      current_line <- current_line + 1
    } else if (current_move == "acc") {
      accumulator <- accumulator + input_tibble$number[current_line]
      current_line <- current_line + 1
    } else if (current_move == "jmp"){
      current_line <- current_line + input_tibble$number[current_line]
    }
  }
  return(list(acc = accumulator,
              curr_line = current_line,
              curr_move = current_move,
              prev_lines = previous_lines))
}

status_at_inf_loop(example_input)
status_at_inf_loop(input)

# Part 2: swap a nop to a jump or a jump to a nop so that the code all runs ----

# Returns FALSE for an infinite loop or accumulator if EOF is reached
accumulator_at_EOF <- function(input_data, line_to_change){
  # Fomat input strings into usable data frame
  input_tibble <- tibble(input_string = input_data) %>%
    separate(input_string, into = c("command", "number"), sep = " ") %>%
    mutate(number = as.integer(number))

  if(input_tibble$command[line_to_change] == "acc")  return(FALSE)
  if(input_tibble$command[line_to_change] == "nop")  input_tibble$command[line_to_change] <- "jmp"
  if(input_tibble$command[line_to_change] == "jmp")  input_tibble$command[line_to_change] <- "nop"

  accumulator <- 0
  current_line <- 1
  previous_lines <- c()

  # Follow flow control until you loop back to a previous step or get to EOF
  while (!(current_line %in% previous_lines) & current_line <= NROW(input_tibble)){
    current_move <- input_tibble$command[current_line]
    previous_lines <- c(previous_lines, current_line)
    if(current_move == "nop"){
      current_line <- current_line + 1
    } else if (current_move == "acc") {
      accumulator <- accumulator + input_tibble$number[current_line]
      current_line <- current_line + 1
    } else if (current_move == "jmp"){
      current_line <- current_line + input_tibble$number[current_line]
    }
  }

  # Are you are the end of the file?
  is_EOF = current_line == (NROW(input_tibble) + 1)
  if(is_EOF) return(accumulator)
  return(is_EOF)
}

# Test on example input
reaches_EOF(input_data = example_input, line_to_change = 2)
map_dbl(.x = 1:9, .f = reaches_EOF, input_data = example_input)
line_to_change = max(map_dbl(.x = 1:9, .f = reaches_EOF, input_data = example_input))

# Use on real input
line_to_change = max(map_dbl(.x = 1:NROW(input), .f = reaches_EOF, input_data = input))
line_to_change

