##d4 playing bingo with a squid

library(tidyverse)
library(readr)
require(reshape)
library(rlang) 


## pull in data
num_called<-read_csv("d4_squid_bingo_num_called.csv")
#test data
#num_called <- data.frame(num_called = c(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)) 
num_called%>%nrow() ## 1000 rows
num_called%>%head()

bingo_cards<- read_csv("d4_squid_bingo_cards.csv")

# #test data
# bingo_cards<- data.frame(bingo_card = c("22 13 17 11  0",
#                                          "8  2 23  4 24",
#                                          "21  9 14 16  7",
#                                          "6 10  3 18  5",
#                                          "1 12 20 15 19",
#                                          "3 15  0  2 22",
#                                          "9 18 13 17  5",
#                                          "19  8  7 25 23",
#                                          "20 11 10 24  4",
#                                          "14 21 16 12  6",
#                                          "14 21 17 24  4",
#                                          "10 16 15  9 19",
#                                          "18  8 23 26 20",
#                                          "22 11 13  6  5",
#                                          "2  0 12  3  7"))
bingo_cards%>%head()

#################### clean card data and split into cards #################### 
bingo_cards$bingo_card<-gsub("  "," ",bingo_cards$bingo_card) #remove any double spaces for single digits

bingo_cards<-bingo_cards%>%
  separate(bingo_card, into = letters[1:5], sep = " ")%>% #split into columns
  mutate_if(is.character,as.numeric)

num_cards = nrow(bingo_cards)/5
card_names<-sprintf("card_%03d", seq(1:num_cards))##define the names of cards

card_num = 1
end_row = 1
## loop over the data and split into cards of 5 rows long and assign them all as df
while(card_num<=num_cards){
  if(card_num ==1) {start_row = 1} else{start_row = end_row+1}
  end_row = start_row+4

  card <- bingo_cards[start_row:end_row,]
  assign(paste0(card_names[card_num]), card) ## write to a variable with the name dynamically made

  card_num = card_num+1
}
rm(card, end_row,card_num, num_cards, start_row)
################################################################################

## get the list of cards
bingo_card_names <- card_names #ls()[grepl("card", ls())]
## get a list of the actual df, not just their names
bingo_card_list <- list()
for (i in 1:length(bingo_card_names)) {
  bingo_card_list[[i]] <- get(bingo_card_names[i])
}
names(bingo_card_list) <- bingo_card_names
bingo_card_list%>%length()


######################## Functions ########################################################
## functions to alter value to NA if it's called
remove_num_called <- function(bingo_card_list, num_called) {
  for (card_num in 1:length(bingo_card_list)) {
    ## change the value and replace the card in the list
    current_card <- bingo_card_list[[card_num]]
    current_card[current_card == num_called] <- NA
    bingo_card_list[[card_num]] <- current_card
  }
  return(bingo_card_list)
}
# current_card<-bingo_card_list[[1]]
# current_card[current_card == 7]<- NA
# bingo_card_list[[1]] <- current_card

## function to check if bingo is met by looking for a row or column with one row/col entirely NA values
check_bingo <- function(bingo_card_list, num) {
  for (card_num in 1:length(bingo_card_list)) {
    ##check if one row/col is entire NA
    row_check <- rowSums(is.na(bingo_card_list[[card_num]]))
    col_check <- colSums(is.na(bingo_card_list[[card_num]]))
    
    ## if a row or column is NA it's hit bingo
    if (5 %in% row_check || 5 %in% col_check) {
      #If it's not already registered put in the list
      if (!card_num %in% completed_card_list) {
        next_item_index <- length(bingo_cards_completed) + 1
        
        bingo_cards_completed[[next_item_index]] <<-bingo_card_list[[card_num]]
        completed_card_list[next_item_index] <<- card_num
        bingo_number[[next_item_index]] <<- num
      }
    }
    
  }
  return()
}


################################# Find bingo ###############################################

bingo_cards_completed = list()
completed_card_list = list()
bingo_number = list()

## run through each card looking for bingo
for (num in 1:nrow(num_called)) { 
  
  bingo_card_list <<- remove_num_called(bingo_card_list,num_called$num_called[num])
  check_bingo(bingo_card_list, num_called$num_called[num])
  
}


### Winning Board
print(paste0())
print(paste0("winning board = ", completed_card_list[1]))
print(paste0("winning number called = ", bingo_number[1]))
print(paste0("card total = " ,bingo_cards_completed[[1]]%>%sum(na.rm=TRUE)))
print(paste0("winning score = ",bingo_cards_completed[[1]]%>%sum(na.rm=TRUE)*bingo_number[[1]]))


### losing Board
print(paste0())
print(paste0("losing board = ", completed_card_list[100]))
print(paste0("losing number called = ", bingo_number[100]))
print(paste0("card total = " ,bingo_cards_completed[[100]]%>%sum(na.rm=TRUE)))
print(paste0("losing score = ",bingo_cards_completed[[100]]%>%sum(na.rm=TRUE)*bingo_number[[100]]))

