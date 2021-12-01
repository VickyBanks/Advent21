library(tidyverse)
library(readr)
## pull in data


## Task one
## how many depths are larger than the previous depth
depths <- read_csv("1_seabed_depths.csv")
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