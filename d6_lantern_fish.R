#d6 lantern fish shoals reproducing
library(tidyverse)
library(readr)
library(ggplot2)
library(ggpmisc)

## Task 1 - how many fish after 80 days
## Task 2 - how many fish after 256 days

initial_fish <- read_csv("d6_lantern_fish.csv")
initial_fish<- data.frame(day_0 = c(3,4,3,1,2)) ##test data

######## Context ########
## On day one, fish have between 0 and 6 days until they produce an new fish.
## The new fish has 8 days before it's first reproduction, and then follows as normal

## Track the number of fish with any given status

fish_status <- 
  data.frame(status = c(8,7,6, 5, 4, 3, 2, 1, 0)) %>%
  left_join(initial_fish %>% group_by(day_0) %>% count(), 
            by = c("status" = "day_0")) %>%
  mutate_all(~ replace(., is.na(.), 0))%>%
  rename(day_0 = n)

fish_status

for(day in 1:256){
fish_status<-
fish_status %>% mutate(!!paste0("day_", (ncol(fish_status)-1)) := 
                         case_when( status == 8 ~ lead(.[[ncol(fish_status)]], n=8), ## the newborn =  previous day's day 0 number
                                    status == 6 ~ lag(.[[ncol(fish_status)]])+lead(.[[ncol(fish_status)]], n=6), # the previous days day 0 return to day 6, plus the newborn counting down from 7 days.
                                    status != 8  & status !=6 ~ lag(.[[ncol(fish_status)]]) # status is one less than day before
                         )
                         )
}
fish_status[,c(1,ncol(fish_status))] ## check final col

## how many fish in total
total_fish<- fish_status[[ncol(fish_status)]]%>%sum()%>%formatC(format="f", big.mark=",", digits=0)
total_fish

## After 80 days there are 373,378
## After 256 days there are 1,682,576,647,495

