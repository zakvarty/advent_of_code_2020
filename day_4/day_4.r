library(tidyverse)
input <- readLines("input.txt")

## Part 0: Pre-processing input
line_count <- length(input)
is_blank <- function(string){string == ""}
doc_count <- sum(map_lgl(input, is_blank)) + 1

# Format so that each document is a single string
docs_formatted <- rep("", doc_count)
doc_number <- 1
for(line in 1:line_count){
  if(is_blank(input[line])){
    doc_number <- doc_number + 1
  } else{
  docs_formatted[doc_number] <- paste(docs_formatted[doc_number], input[line])
  }
}

## Part 1: Check for all required fields on document
# Function to check if a docstring is a valid passport
valid_document <- function(doc_string){
required_fields <-str_detect(string = doc_string, pattern = c("byr","iyr","eyr","hgt","hcl","ecl","pid"))
all(required_fields)
}

valid_document_count <- sum(map_lgl(docs_formatted, valid_document))

## Part 2: Stricter conditions
valid_document <- function(doc_string, verbose = TRUE){
  if(verbose) print(doc_string)
  valid_byr <- str_detect(doc_string, regex("byr:(19[2-9]\\d|200[0-2])\\b"))
  valid_iyr <- str_detect(doc_string, regex("iyr:(201\\d|2020)\\b"))
  valid_eyr <- str_detect(doc_string, regex("eyr:(202\\d|2030)\\b"))
  valid_hgt <- str_detect(doc_string, regex("hgt:(1[5-8]\\dcm|19[0-3]cm|59in|6\\din|7[0-6]in)\\b"))
  valid_hcl <- str_detect(doc_string, regex("hcl:#(\\d|[a-f]){6}\\b"))
  valid_ecl <- any(str_detect(doc_string, paste0("ecl:",c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"), "\\b")))
  valid_pid <- str_detect(doc_string, regex("pid:\\d{9}\\b"))

  field_names <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  field_valid <- c(valid_byr, valid_iyr, valid_eyr, valid_hgt, valid_hcl, valid_ecl, valid_pid)
  if(verbose){
    for(i in 1:7){
      print(paste(field_names[i], ",", field_valid[i]))
    }
  }

  all(field_valid)
}

valid_document(docs_formatted[7], verbose = TRUE)
valid_document_count <- sum(map_lgl(docs_formatted, valid_document, verbose = FALSE))

