## AOC 2020 - Day 11: Seating System ------------------------------------------

## -----------------------------------------------------------------------------
## Part 0: Load data and functions --------------------------------------------
## -----------------------------------------------------------------------------

library(tidyverse)
#input <- readLines("~/GitHub/advent_of_code_2020/day_11/test_input.txt")
input <- readLines("~/GitHub/advent_of_code_2020/day_11/input.txt")

# Format input into matrix and pad with "."
layout <- do.call(rbind, str_split(input, pattern =  ''))
layout <- rbind(".", cbind(".", layout, "."), ".")

## -----------------------------------------------------------------------------
## Part 1: Change seat occupancy based on number of occupied neighbours. -------
## When this converges, how many occupied seats are there? ---------------------
## -----------------------------------------------------------------------------

get_next_layout <- function(current_layout){
  nrows <- NROW(layout)
  ncols <- NCOL(layout)
  next_layout <- current_layout

  for(i in 2:(nrows-1)){
    for(j in 2:(ncols-1)){
      ### FOR EACH LOCATION
      this_spot <- current_layout[i,j]
      if(this_spot != "."){
        # count the number of neighbours
        neighbours <- current_layout[(i-1):(i+1),(j-1):(j+1)]
        neighbour_count <- sum(neighbours[-5] == "#")
        # Change seat if needed
        if ((this_spot == "L") & (neighbour_count == 0)){
          next_layout[i,j] <- "#"
        }
        if ((this_spot == "#") & (neighbour_count >= 4)){
          next_layout[i,j] <- "L"
        }
      }
      ### END
    }
  }
  return(next_layout)
}

# First update
iterations <- 0
current_state <- layout
next_state <- get_next_layout(current_state)
iterations <- iterations +1
# Keep coing until convergence:
while(any(current_state != next_state) ){
  current_state <- next_state
  next_state <- get_next_layout(current_state)
  iterations <- iterations + 1
  n_different <- sum(current_state != next_state)
  print(c(iterations, n_different))
}

ans_1 <- sum(current_state == "#")

## -----------------------------------------------------------------------------
### PART 2: Decison rules chaged, based on Queen neighbour count and max 5
### occupied. How many occupied seats at convergence now?
## -----------------------------------------------------------------------------

# Source functions to retrive character in each line of sight
source("~/GitHub/advent_of_code_2020/day_11/get_neighbour_functions.r")

# Counts number of neighbours for a given location
count_line_of_sight <- function(current_layout, row, col, n_rows, n_cols){
  neighbours <- rep(NA, 8)
  neighbours[1] <- get_NN_neighbour(current_layout, row, col,n_rows, n_cols)
  neighbours[2] <- get_NE_neighbour(current_layout, row, col,n_rows, n_cols)
  neighbours[3] <- get_EE_neighbour(current_layout, row, col,n_rows, n_cols)
  neighbours[4] <- get_SE_neighbour(current_layout, row, col,n_rows, n_cols)
  neighbours[5] <- get_SS_neighbour(current_layout, row, col,n_rows, n_cols)
  neighbours[6] <- get_SW_neighbour(current_layout, row, col,n_rows, n_cols)
  neighbours[7] <- get_WW_neighbour(current_layout, row, col,n_rows, n_cols)
  neighbours[8] <- get_NW_neighbour(current_layout, row, col,n_rows, n_cols)
  #print(neighbours)
  return(sum(neighbours == "#"))
}

# Example:
# count_line_of_sight(current_layout = layout, row = 1, col = 5, n_rows = 12, n_cols = 12)

get_next_layout <- function(current_layout){
  nrows <- NROW(layout)
  ncols <- NCOL(layout)
  next_layout <- current_layout

  for(i in 2:(nrows-1)){
    for(j in 2:(ncols-1)){
      ###
      this_spot <- current_layout[i,j]
      if(this_spot != "."){
        # count the number of neighbours
        neighbour_count <- count_line_of_sight(current_layout, i, j, nrows, ncols)
        # Change seat if needed
        if ((this_spot == "L") & (neighbour_count == 0)){
          next_layout[i,j] <- "#"
        }
        if ((this_spot == "#") & (neighbour_count >= 5)){
          next_layout[i,j] <- "L"
        }
      }
      ###
    }
  }
  return(next_layout)
}

# Do first update
iterations <- 0
current_state <- layout
next_state <- get_next_layout(current_state)
iterations <- iterations +1
# Keep going until converged
while(any(current_state != next_state) ){
  current_state <- next_state
  next_state <- get_next_layout(current_state)
  iterations <- iterations + 1
  n_different <- sum(current_state != next_state)
  print(c(iterations, n_different))
}

ans_2 <- sum (current_state == "#")

