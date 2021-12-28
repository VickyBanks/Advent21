#d7 crab submarines helping you avoid a Whale
library(tidyverse)
library(readr)

## Task - the crabs must move to align with one position moved using one unit of fuel. 
## You need to find the position that uses the least fuel for all crabs to get there
## Task 1 - one move = one until of fuel
## Task 2 - each move uses one more unitl of fuel i.e first move = 1, second move = 2, so to move 2 positions uses 1+2=3 units

crabs<- data.frame(position = c(16,1,2,0,4,2,7,1,2,14))
crabs<- read_csv("d7_hungry_whale.csv")
crabs%>%head()

## max and min positions
max_pos<-crabs$position %>%max() #16
min_pos<-crabs$position %>%min() #0
print(max_pos)
print(min_pos)


fuel_total_per_pos<-data.frame(position = as.numeric(), fuel = as.numeric())

## for each possible position
for(align_position in min_pos:max_pos) {
  fuel_total <- 0
  ## find the fuel for each crab to get there
  for (crab in 1:nrow(crabs)) {
    
    moves<-abs(crabs$position[crab] - align_position)
    fuel_used <- 0.5*(moves*moves + moves)
    fuel_total <- fuel_total + fuel_used
  }
  ##put in df
  fuel_total_per_pos <- 
    fuel_total_per_pos %>%
    rbind(data.frame(position = align_position, fuel = fuel_total) )
}

## which positions gives smallest fuel?
fuel_total_per_pos %>%filter(fuel == min(fuel_total_per_pos$fuel))


