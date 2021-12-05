library(tidyverse)
library(readr)
require(reshape)
## pull in data
diag <- read_csv("d3_binary_diagnostic.csv")
diag%>%head()
diag%>%nrow() ## 1000 rows

## Task one - for each bit in the binary, add all the values and find how many are 1 and 0
## use the most and least common to create new binary 'diagnositic' values called gamma and epsilon

## split the binary into individual bits and sum them
diag_split <-
  diag %>% 
  transform(value = colsplit(value, split = "", names = letters[1:12])) %>%
  colSums() %>%
  data.frame() %>%
  rownames_to_column() %>%
  dplyr::rename(sum_value = 2) %>%
  ##the gamma value is the bit value that is found more often in that position.
  ## epsilon rate is the least common
  ## as there are 1000 rows, if the sum >500 then 1 was most common, else 0
  mutate(
    gamma_bit_value = case_when(sum_value >= nrow(diag)/2 ~ 1, #nrows(diag)/2 is half the values = 500
                                sum_value < nrow(diag)/2  ~ 0),
    epsilon_bit_value = case_when(sum_value >= nrow(diag)/2 ~ 0,
                                  sum_value < nrow(diag)/2  ~ 1)
  )
diag_split

#combine into one binary value
gamma = as.character()
epsilon = as.character()
for(row in 1:nrow(diag_split)){
  gamma = paste0(gamma,diag_split$gamma_bit_value[row] )
  epsilon = paste0(epsilon,diag_split$epsilon_bit_value[row] )
}

#https://coolconversion.com/math/binary-octal-hexa-decimal/_binary_number_11011011_to_decimal_
gamma #111011010101
epsilon
##turn into decimal equivalents
gamma_decimal =  3797
epsilon_decimal = 298

# What is the power consumption of the submarine?
gamma_decimal*epsilon_decimal #1,131,506


### part 2 - oxygen & co2 rating 
##find the most/least common value in position 1, and filter the list to keep only those with that value in that position.
#then repeat for all positions

##function to find the most/least common value (0 or 1) in each column
value_check_fun <- function(df) {
  value_check_df <<-
    df %>%
    colSums() %>%
    data.frame() %>%
    rownames_to_column(var = "rowname") %>%
    dplyr::rename(sum_value = 2) %>%
    mutate(most_common = case_when(
      sum_value >= nrow(df) / 2 ~ 1,
      sum_value < nrow(df) / 2  ~ 0
    ))%>%
    mutate(least_common = case_when(
      sum_value >= nrow(df) / 2 ~ 0,
      sum_value < nrow(df) / 2  ~ 1
    ))
  
  return(value_check_df)
}

##take intial diagnostic data and split each bit into a new column
diag_split_df <-
  diag %>% 
  separate(value, into = letters[1:13], sep = "") %>%
  select(-a)%>%
  mutate_if(is.character,as.numeric)

diag_split_df


## make one df for oxygen count and one for co2 count
co2_df <- diag_split_df
oxygen_df <- diag_split_df

##loop over each bit in the 12 bit value re-assessing the most/least common value in each position as the df are filtered
for (n in 1:ncol(diag_split_df)) {
  
  ## find the most common value in each position
  oxygen_check_df <- value_check_fun(oxygen_df)
  co2_check_df <- value_check_fun(co2_df)
  
  
  ## filter data set to only keep rows with that value
  print(paste0("most common value = ", value_check_df[n, 3]))
  #print(paste0("least common value = ", value_check_df[n, 4]))
  if (nrow(oxygen_df) > 1) {
    oxygen_df <-
      oxygen_df %>%
      filter_at(c(n), all_vars(. == oxygen_check_df[n, 3]))
    print(paste0(nrow(oxygen_df), " rows left in df"))
  }
  
  if (nrow(co2_df) > 1) {
    co2_df <-
      co2_df %>%
      filter_at(c(n), all_vars(. == co2_check_df[n, 4]))
    print(paste0(nrow(co2_df), " rows left in df"))
    
  }
}

oxygen_df
co2_df

#combine into one binary value
oxygen_rating<- as.character()
co2_rating<- as.character()
for(col in 1:ncol(diag_split_df)){
  co2_rating = paste0(co2_rating,co2_df[[col]] )
  oxygen_rating = paste0(oxygen_rating,oxygen_df[[col]] )
}
oxygen_rating #111111111001
co2_rating #011110000011


#https://coolconversion.com/math/binary-octal-hexa-decimal/_binary_number_11011011_to_decimal_
oxygen_rating_decimal = 4089
co2_rating_decimal = 1923

# What is the life support rating of the submarine?
oxygen_rating_decimal * co2_rating_decimal #7863147








