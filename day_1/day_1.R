library(readr)
library(dplyr)

## PART 1
input <- read_csv("~/GitHub/advent_of_code_2020/day_1/input.txt", col_names = "entries") %>% pull(entries)
input_length <- length(input)
for(i in 1:(input_length - 1)){
  for(j in (i+1):input_length){
    if(input[i] + input[j] == 2020){
      print(i)
      print(j)
    }
  }
}
input[143] + input[166]
input[143] * input[166]

## PART 2
for(i in 1:(input_length - 3)){
  for(j in (i+1):(input_length - 2)){
    for(k in (j+1): (input_length - 1)){
      if(input[i] + input[j] + input[k] == 2020){
        print(i)
        print(j)
        print(k)
      }
    }
  }
}
input[62] + input[77] + input[100]
input[62] * input[77] * input[100]
