#d8_display_board
## task the display board show numbers by lighting up different horizontal/vertical 
## groups of lights called segments. The segments are labelled a-g. 


# num_of_seg	num_displayed	definate_seg_letters	possible_seg_letters
#     2	          1	                cf	
#     3	          7	               acf	
#     4	          4	              bcdf	
#     7	          8	           abcdefg	
#     5	      2,3,5	               adg	            cbef
#     6	      0,6,9	              abfg	             cde

library(tidyverse)
library(readr)

################## read in data ##################
data<- read_csv("d8_display_board.csv")
#data<- read_csv("d8_test_data.csv")
data%>%head()


## split the data into the patterns for the numbers (signal patterns)
## and the numbers observed (output patterns)
## each row is a new instance of the patterns
signal_patterns<- 
  data %>% 
  select(signal_patterns)%>%
  separate(signal_patterns, into = letters[17:26], sep = " ") #split into columns
signal_patterns %>%head()

output_patterns<- 
  data %>%
  select(output)%>%
  separate(output, into = letters[23:26], sep = " ")
output_patterns%>%head()



## the known numbers are those with length 2,3,4,8
## how many are there in the outputs given?
count_known_num = 0

for (row in 1:nrow(signal_patterns)) {
  known_numbers <-
    output_patterns[row, ] %>%
    gather(key = dummy_var, value = signal) %>%
    select(-dummy_var) %>%
    mutate(signal_length = sapply(signal, nchar)) %>%
    mutate(
      num_value = case_when(
        signal_length == 2 ~ 2,
        signal_length == 3 ~ 7,
        signal_length == 4 ~ 4,
        signal_length == 7 ~ 8
      )
    ) %>%
    filter(!is.na(num_value))
  
  count_known_num = count_known_num + nrow(known_numbers)
}
count_known_num


# known_patterns <-
#   signal_patterns[1, ] %>%
#   gather(key = dummy_var, value = signal) %>%
#   select(-dummy_var) %>%
#   mutate(signal_length = sapply(signal, nchar)) %>%
#   mutate(
#     num_value = case_when(
#       signal_length == 2 ~ 2,
#       signal_length == 3 ~ 7,
#       signal_length == 4 ~ 4,
#       signal_length == 7 ~ 8
#     )
#   ) %>%
#   filter(!is.na(num_value))
# 
# known_patterns