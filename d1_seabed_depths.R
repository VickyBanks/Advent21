library(tidyverse)
library(readr)
## pull in data


## Task one
## how many depths are larger than the previous depth
depths <- read_csv("d1_seabed_depths.csv")
depths%>%head()

depths %>%
  mutate(previous_depth = lag(Depths))%>%
  mutate(comp = case_when(Depths > previous_depth ~ "larger",
                          Depths < previous_depth ~ "smaller"
                          ))%>%
  group_by(comp)%>%
  count()

## 1681 are larger
## 318 are smaller

## Task 2
## use three measurements to find a total and find if that value is increasing
depths %>%
  mutate(depth_2 = lead(Depths),
         depth_3 = lead(depth_2),
         total = Depths + depth_2 + depth_3
         )%>%
  mutate(comp = case_when(total > lag(total) ~ "larger",
                          total < lag(total)  ~ "smaller",
                          total == lag(total)  ~ "same"
         ))%>%
  group_by(comp)%>%
  count()
## 1704 are larger
## 265 are smaller
