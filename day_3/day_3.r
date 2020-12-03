# --- Day 3: Toboggan Trajectory ---

library(tidyverse)

mountain <- read_csv(file = "~/GitHub/advent_of_code_2020/day_3/input.txt",col_names = FALSE)
col_max <- str_length(mountain$X1[1])
is_tree <- function(row,col){substr(mountain$X1[row], start = col , stop = col) == "#"}

# Part 1: Number of trees hit using steps of (3,1)
tree_counter <- function(right_step_size, down_step_size){
  current_row <- 1
  current_col <- 1
  tree_count <- 0
  row_step <- down_step_size # how far down with each step
  col_step <- right_step_size # how far across with each step
  while(current_row < NROW(mountain)){
    # Take a step
    current_row <- current_row + row_step
    current_col <- current_col + col_step
    # loop around if you go off the right
    if(current_col > col_max){
      current_col <- current_col %% col_max
    }
    if(is_tree(current_row, current_col)){
      tree_count <- tree_count + 1
    }
    #print(c(current_row, current_col))
    #print(tree_count)
  }
  return(tree_count)
}
tree_counter(3,1)

# Part 2: Product of the number of trees hit using each of:
# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.
prod(tree_counter(1,1),
     tree_counter(3,1),
     tree_counter(5,1),
     tree_counter(7,1),
     tree_counter(1,2))

