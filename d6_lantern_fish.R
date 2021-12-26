#d6 lantern fish shoals reproducing
library(tidyverse)
library(readr)


## Task - every 7 days they reproduce one individual. 
## This individual has an additional 2 days before their 7 day count commences.
## Task 1 - how many fish after 80 days

initial_fish<-read_csv("d6_lantern_fish.csv")
initial_fish<- data.frame(day_0 = c(3,4,3,1,2)) ##test data

fish<-initial_fish


for(day in 1:256){
  
  ## add rows for new fish
  num_new_fish <- sum(fish[[ncol(fish)]]==0)
  if (num_new_fish > 0) {
    new_rows <- 
      fish[1:num_new_fish,] %>%
      replace(, NA)
    fish <- fish %>% rbind(new_rows)
  }
  
  
  ## add new day column
  fish <- fish %>% 
    mutate(!!paste0("day_",ncol(fish)) :=  case_when(.[[ncol(fish)]] == 0 ~ 6,
                                                     is.na(.[[ncol(fish)]]) ~8,
                                                 .[[ncol(fish)]] !=0 ~ .[[ncol(fish)]]-1))

}

print(paste0("num_fish = ", nrow(fish)))

## after 80 days there are 373,378

##after 256 days for test data fish = 26,984,457,539

# day<-0
# fish<-as.list(fish)
# for(day in 1:3){
#   day = day+1
#   num_new_fish <- sum(fish[[ncol(fish)]]==0)
#   
#   
#   
#   for (new_fish in 1:num_new_fish){
#     fish.append(8)
#   }
# 
#    
# }


