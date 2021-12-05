library(tidyverse)
library(readr)
require(reshape)
## pull in data


## Task one - for each bit in the binary, add all the values and find how many are 1 and 0
diag <- read_csv("d3_binary_diagnostic.csv")
diag%>%head()
diag%>%nrow() ## 1000 rows

## split the binary into individual bits and sum
diag_split <-
  diag %>% 
  transform(value = colsplit(value, split = "", names = letters[1:12])) %>%
  colSums() %>%
  data.frame() %>%
  rownames_to_column() %>%
  dplyr::rename(sum_value = 2) %>%
  
  ##the gamma value is which every bit is found more often in that position.
  ## epsilon rate is the least common
  ## as there are 1000 rows, if the sum >500 then 1 was most common, else 0
  mutate(
    gamma_bit_value = case_when(sum_value >= nrow(diag)/2 ~ 1, #nrows(diag)/2 is half the values = 500
                                sum_value < nrow(diag)/2  ~ 0),
    epsilon_bit_value = case_when(sum_value >= nrow(diag)/2 ~ 0,
                                  sum_value < nrow(diag)/2  ~ 1)
  )
diag_split

gamma = as.character()
epsilon = as.character()
for(row in 1:nrow(diag_split)){
  gamma = paste0(gamma,diag_split$gamma_bit_value[row] )
  epsilon = paste0(epsilon,diag_split$epsilon_bit_value[row] )
}

#https://coolconversion.com/math/binary-octal-hexa-decimal/_binary_number_11011011_to_decimal_
gamma #111011010101
epsilon
gamma_decimal =  3797
epsilon_decimal = 298

# What is the power consumption of the submarine?
gamma_decimal*epsilon_decimal #1,131,506




