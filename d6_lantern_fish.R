#d6 lantern fish shoals reproducing
library(tidyverse)
library(readr)
library(ggplot2)
library(ggpmisc)

## Task - every 7 days they reproduce one individual.
## This individual has an additional 2 days before their 7 day count commences.
## Task 1 - how many fish after 80 days
## Task 2 - how many fish after 256 days

initial_fish <- read_csv("d6_lantern_fish.csv")
#initial_fish<- data.frame(day_0 = c(3,4,3,1,2)) ##test data

### function to add a new row when a new fish is born
new_fish <- function(fish) {
  num_new_fish <- sum(fish[[ncol(fish)]] == 0)
  if (num_new_fish > 0) {
    new_rows <-
      fish[1:num_new_fish,] %>%
      replace(, NA)
    fish <- fish %>% rbind(new_rows)
  }
  return(fish)
}

## On day one, fish have between 0 and 6 days until the produce an new fish.
## Therefore, find how many that starting value produces after x days.
### then find how many fish initially have that starting day value and multiply

## blank df for the number of fish produced
fish_produced <- data.frame(day_count = as.double(),
                            fish_produced = as.numeric())
fish_produced

## df to give the starting staus of the fish
fish_start_status<- data.frame(day_0 = c(0,1,2,3,4,5,6))
fish_start_status


for(n in 1:nrow(fish_start_status)) {
  print(paste0("start_status = ",fish_start_status[n,]))
  fish <- fish_start_status %>% slice(n)
  for (day in 1:80) {
    ##add new fish born each day
    fish <- new_fish(fish)
    
    ## add new day column
    fish <- fish %>%
      mutate(!!paste0("day_", ncol(fish)) :=
               case_when(.[[ncol(fish)]] == 0 ~ 6,
                         is.na(.[[ncol(fish)]]) ~ 8,
                         .[[ncol(fish)]] != 0 ~ .[[ncol(fish)]] - 1))
  }
  
  ## collect data in df
  fish_produced <- fish_produced %>% rbind(data.frame(day_count = fish_start_status[n,],
                                                      fish_produced = nrow(fish)))
}

print(fish_produced)


## find number of fish with each starting status
num_fish_starting_status<- initial_fish %>% group_by(day_0) %>% count()
num_fish_starting_status

##Join the num fish produced given each starting status
total_fish <- fish_produced %>%
  left_join(num_fish_starting_status,
            by = c("day_count" = "day_0")) %>%
  mutate_all(~ replace(., is.na(.), 0)) %>%
  mutate(total_fish = fish_produced * n)

print(total_fish)

## find total fish at end of the period
total_fish$total_fish %>% sum()

## after 80 days there are 373,378


################ Task 2 - 256 days is too long to compute this way ################
##data intially produced in part 1
fish<-initial_fish
for(day in 1:80){
  
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

#### can we work out an equation?? 
daily_fish<-data.frame(day = 0, fish = colSums(!is.na(fish[1])))
for(day in 1:(ncol(fish)-1)) {
  daily_fish <-
    daily_fish %>%
    rbind(data.frame(day = day, fish = colSums(!is.na(fish[(day+1)]))))
}

daily_fish<-daily_fish%>%mutate(fish_gained =fish-300)
head(daily_fish)
#write_csv(daily_fish, file = "daily_fish.csv")

###is the rate exponential?
ggplot(data = daily_fish, aes(x = day, y = fish)) +
  geom_point()

## log of the value will give a straight line graph
ggplot(data = daily_fish%>%filter(day >10), aes(x = day, y = log(fish))) +
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  stat_poly_eq(formula = y~x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE)

### current equation
# y = a * exo(r*x) 
# fish = const1 * exp(const2 * day) + starting_num
# 
# fish_gained  = const1*exp(const2 * day)
# fish_gained/const1 = exp(const2 * day)
# ln(fish_gained) = const2 * day + ln(const1)

## y = 5.85 +0.0876x
## log(fish_gained) = log(const) + r*day
## log(fish_gained) = 5.72 + 0.089*day

day = 256
log_fish_gained = 5.85 + 0.087*day
log_fish_gained
exp(log_fish_gained) ##321,258.1 from model for 80 days but real answer =373,378
(log(373078)-5.85)/0.087 #= log(const) + const*day











## seems too inaccurate






