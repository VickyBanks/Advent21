##d4 playing bingo with a squid

library(tidyverse)
library(readr)
require(reshape)
library(rlang) 


## pull in data
num_called<-read_csv("d4_squid_bingo_num_called.csv")
#test data
num_called <- data.frame(num_called = c(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)) 
num_called%>%nrow() ## 1000 rows
num_called%>%head()

bingo_cards<- read_csv("d4_squid_bingo_cards.csv")

#test data
bingo_cards<- data.frame(bingo_card = c("22 13 17 11  0",
                                         "8  2 23  4 24",
                                         "21  9 14 16  7",
                                         "6 10  3 18  5",
                                         "1 12 20 15 19",
                                         "3 15  0  2 22",
                                         "9 18 13 17  5",
                                         "19  8  7 25 23",
                                         "20 11 10 24  4",
                                         "14 21 16 12  6",
                                         "14 21 17 24  4",
                                         "10 16 15  9 19",
                                         "18  8 23 26 20",
                                         "22 11 13  6  5",
                                         "2  0 12  3  7"))
bingo_cards%>%head()

########## clean card data and split into cards ##########
bingo_cards$bingo_card<-gsub("  "," ",bingo_cards$bingo_card) #remove any double spaces for single digits

bingo_cards<-bingo_cards%>%
  separate(bingo_card, into = letters[1:5], sep = " ")%>% #split into columns
  mutate_if(is.character,as.numeric)

num_cards = nrow(bingo_cards)/5
card_names<-sprintf("card_%03d", seq(1:100))
card_num = 1
end_row = 1
while(card_num<=num_cards){
  if(card_num ==1) {start_row = 1} else{start_row = end_row+1}
  end_row = start_row+4
  
  
  card = bingo_cards[start_row:end_row,]
  assign(paste0(card_names[card_num]), card) ## write to a variable with the name dynamically made

  card_num = card_num+1
}

########################################

## get a list of cards
bingo_card_names <- ls()[grepl("card_0", ls())]
#bingo_card_names<-bingo_card_names %>%append("card_100")
## get a list of the actual df, not just their names
bingo_card_list <- list()
for (i in 1:length(bingo_card_names)) {
  bingo_card_list[[i]] <- get(bingo_card_names[i])
}
names(bingo_card_list) <- bingo_card_names
bingo_card_list%>%length()


 ## functions to alter value to 0 if it's called
remove_num_called<-function(card, num_called ){
  card[card==num_called]<-0
  return(card)
}
## function to check if bingo is met by looking for a row or column with the sum = 0
check_bingo_win <- function(card, card_name) {
  if (!is.na(match(0, colSums(card))) ||
      !is.na(match(0, rowSums(card)))) {
    print("bingo!")
    print(card_name)
    print(card)
    winning_board <<- card
    break
  }
}
##if looking for the loosing board, remove each one as it wins
check_bingo_lose <- function(card, card_name, card_position) {
  if (!is.na(match(0, colSums(card))) ||!is.na(match(0, rowSums(card)))) {
    if(length(bingo_card_list)!=1){##don't remove if it's the final one
    bingo_card_list <- bingo_card_list[-card_position] ## remove the card that just won
    print(paste0(card_name, ' removed') )
    }
  }
  return(bingo_card_list)
}
call_count = 0
##loop over each number called for each df, cross out the value if found, then check for bingo
for (num in 1:nrow(num_called)) {
  call_count = call_count + 1
  print(paste0("nummber called =", num_called$num_called[num]))
  
  for (card in 1:length(bingo_card_list)) {
    print(paste0("on card ", card, " of ", length(bingo_card_list), ' remaining'))
    
    if (length(bingo_card_list) >= card) {
      bingo_card_list[[card]] <-
        remove_num_called(bingo_card_list[[card]], num_called$num_called[num])

      print(bingo_card_list[[card]])
      
      bingo_card_list <<-
        check_bingo_lose(bingo_card_list[[card]], bingo_card_names[[card]], card)
    }
    print(paste0(length(bingo_card_list), ' remaining'))
    
    # if (length(bingo_card_list) == 1 & card ==1) {## if this is the final card
    #   print(bingo_card_list[[1]])
    #   ##and it's also hit bingo
    #   if (!is.na(match(0, colSums(bingo_card_list[[card]]))) ||
    #       !is.na(match(0, rowSums(bingo_card_list[[card]])))){
    #       
    #       stop = TRUE
    #       print(paste0("Loser is ", names(bingo_card_list)))
    #       print(bingo_card_list)
    #       losing_board <<- bingo_card_list[[1]]
    #       break
    #     }
    # }
   if(isTRUE(stop)){break} 
  }
  if(isTRUE(stop)){break} 
}

### winning score
# board_sum<-Reduce("+",winning_board%>%colSums())
# 
# winning_score<- num_called$num_called[num] * board_sum
# winning_score #55770
# 

### losing board
# board_sum<-Reduce("+",losing_board%>%colSums())
# losing_score<- num_called$num_called[num] * board_sum
# losing_score

