library(tidyverse)
library(readr)
## pull in data


## Task one - find the final position horizontally and vertically
motion <- read_csv("d2_sub_motion.csv")
motion%>%head()

##quick look
motion$direction%>%unique()
motion %>%filter(direction == "forward")%>%summary()##all positive
motion %>%filter(direction != "forward")%>% group_by(direction) %>%count() ## 378 down and 206 up


##horizontal - 2018
horiz<- motion %>%filter(direction == "forward")%>%summarise(horiz = sum(value))##2018

##vertical - 820
depth <- motion %>% 
  filter(direction != "forward")%>%
  mutate(value = case_when(direction  == 'down' ~ value,
                           direction  == 'up' ~ -value))%>%
  summarise(depth = sum(value))

##depth*horiz
horiz*depth # 1,654,760


## Task 2 - find the position given that the meaning of the commands has changed
#forward still moves horizontally but also increases your depth by aim * forward value

motion<-
  motion %>%
  mutate(value = case_when(direction  == 'down' ~ value,
                           direction  == 'forward' ~ value,
                           direction  == 'up' ~ -value))

#empty df to have current position after each instruction
position = data.frame("horiz" = 0,"aim" = 0,"depth"= 0)  

for(row in 1:nrow(motion)) {
  current_horiz = position$horiz[nrow(position)]
  current_aim = position$aim[nrow(position)]
  current_depth = position$depth[nrow(position)]
  
  if (motion$direction[row] == 'up' || motion$direction[row] == "down") {
    
    pos_change = data.frame("horiz" = current_horiz,
                            "aim" = current_aim+motion$value[row],
                            "depth" = current_depth
                            )
    position <- position %>% rbind(pos_change)
    
  }
  else if (motion$direction[row] == 'forward') {
    pos_change = data.frame("horiz" = current_horiz + motion$value[row],
                            "aim" = current_aim,
                            "depth" =current_depth + (current_aim * motion$value[row]) #aim is depth + (current aim * forward value)
    )
    position <- position %>% rbind(pos_change)
  }
  
}
position[nrow(position),]
##depth*horiz
position$horiz[nrow(position)]*position$depth[nrow(position)] # 4510

