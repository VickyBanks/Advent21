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
library(stringi)
library(textclean)

################## read in data ##################
data<- read_csv("d8_display_board.csv")
#data<- read_csv("d8_test_data.csv")
# data<- data.frame(signal_patterns = as.character("ab dab eafb cdfbe gcdfa fbcad cefabd cdfgeb cagedb acedgfb"),
#                    output = as.character("cdfeb fcadb cdfeb cdbaf"))
data%>%head()

true_signal_nums <- data.frame(
  true_value = c(1,7,4,2,3,5,0,6,9,8),
  true_signal = c('cf','acf','bcdf','acdeg','acdfg','abdfg','abcefg','abdefg','abcdfg','abcdefg')
  
)
output_value<-c()


############## functions ############## 
##to order the letters
striHelper <- function(x){
  stri_c(x[stri_order(x)], collapse = "")}

order_letters <- function(str_input) {
  ordered_string <-
    vapply(stri_split_boundaries(str_input, type = "character"),
           striHelper,
           "")
  ordered_string <- data.frame(signal = ordered_string)
  return (ordered_string)
}

get_signal <- function(all_signals, signal_num) {
  current_signal <-
    all_signals[signal_num,] %>%
    gather(key = dummy_var, value = signal) %>%
    select(-dummy_var) %>%
    mutate(order_letters(signal))
  
  return(current_signal)
}

find_letter <- function(string_a, string_b) {
  letters_to_match <- as.list(strsplit(string_b$signal[1], "")[[1]])
  string_a$letter <- string_a$signal[1]
  
  for (i in 1:length(letters_to_match)) {
    string_a$letter[1] <-
      gsub(letters_to_match[i], "", string_a$letter)
  }
  return(string_a)
}

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

for(n in 1:nrow(data)){
################## get the signal instance ##################

output<-get_signal(output_patterns, n)
signal <- get_signal(signal_patterns, n) %>%
  mutate(signal_length = sapply(signal, nchar)) %>%
  arrange(signal_length)


################## decode the letters ##################
## set empty df
decode <- data.frame(true_value = as.character(),
                     code_value = as.character())

## the straightforward ones which can only be one signal
number_1<-signal%>%filter(signal_length==2)
number_7<-signal%>%filter(signal_length==3)
number_4<-signal%>%filter(signal_length==4)

decode <- decode %>%
  rbind(data.frame(
    true_value = c("cf", "a", "bd"),
    code_value = c(
      number_1$signal[1],
      mgsub(number_7$signal[1], unlist(str_split(number_1$signal, "")), ""),
      mgsub(number_4$signal, unlist(str_split(number_1$signal, "")), "")
      )
  ))


## use the ones with length 5 to find the final values
#this one includes cf (found from number 1)
letters_to_find <- unlist(str_split(decode$code_value[1],""))
length_5_val_1 <-
  signal %>%
  filter(signal_length == 5) %>%
  filter(grepl(letters_to_find[1], signal)) %>%
  filter(grepl(letters_to_find[2], signal))


## this one includes the bd value (found from number 4)
letters_to_find <- unlist(str_split(decode$code_value[3],""))
length_5_val_3 <-
  signal %>%
  filter(signal_length == 5) %>%
  filter(signal != length_5_val_1$signal) %>%
  filter(grepl(letters_to_find[1], signal) & grepl(letters_to_find[2], signal))

## this is the final one
length_5_val_2<-
  signal%>%
  filter(signal_length==5)%>%
  filter( signal!=length_5_val_1$signal & signal!=length_5_val_3$signal )



## use these three strings to find all the other values individually
length_5_val_1 <- find_letter(length_5_val_1, length_5_val_3)
length_5_val_3 <- find_letter(length_5_val_3, length_5_val_1)

if(length_5_val_1$letter[1]== decode$code_value[1]){
  code_val_c = length_5_val_1$letter[1]
  code_val_b = length_5_val_3$letter[1]
  
} else { 
  code_val_c = length_5_val_1$letter[1]
  code_val_b = length_5_val_3$letter[1]
}
## set b and c
decode <- decode %>%rbind(data.frame(true_value = c("c", "b"),code_value =  c(code_val_c, code_val_b)))

## now f and d can be found
decode <- decode %>%
  rbind(data.frame(true_value = c("f", "d"),
                   code_value = c(gsub(decode$code_value[4], "", signal$signal[1]),
                                  gsub(decode$code_value[5], "", decode$code_value[3]))))


## to find the final letters do the same but with the second length 5 string
### find the letter in string 1 not in string 2
length_5_val_1 <- find_letter(length_5_val_1, length_5_val_2)
length_5_val_2 <- find_letter(length_5_val_2, length_5_val_1)


## idenfity value e
if (length_5_val_1$letter[1] %in% decode$code_value) {
  code_val_e = length_5_val_2$letter[1]
} else {
  code_val_e = length_5_val_1$letter[1]
}
decode <- decode %>% rbind(data.frame(true_value = "e",code_value =  code_val_e))

## identify the missing value and assign it to be g
decode <- decode%>%
  filter(nchar(true_value)==1)%>%
  full_join(data.frame(letters = letters[1:7]),
            by = c("code_value"="letters"))%>%
  mutate_all(~replace(., is.na(.), 'g'))%>%
  arrange(true_value)

################## decode the strings to find which value is which letter ##################

signal$decoded<-as.character("x")

for(value in 1:nrow(signal)){
  val<- as.character()
  for(letter in 1:nrow(decode)){
    if(grepl(decode$code_value[letter],signal$signal[value])){
      val <-paste0(val,decode$true_value[letter])
    }

  }
  signal$decoded[value]<-val

}

decoded_signal<-signal %>%mutate(order_letters(decoded)%>%rename(decoded = signal) )

decoded_pattern<-
  output %>%
  left_join(decoded_signal, by = "signal")%>%
  left_join(true_signal_nums, by = c("decoded" = "true_signal" ))

output_value<-output_value %>%append(as.numeric(paste(decoded_pattern$true_value, sep="", collapse="")))

}

output_value
output_value%>%sum()

# 
# 
# 
# 
# 
# 
