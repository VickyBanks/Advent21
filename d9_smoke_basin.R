##d9 smoke basin

library(tidyverse)
library(readr)

############ Read in data and prep ##############
# data<-data.frame(
#   V1 = c(2199943210,
#              3987894921,
#              9856789892,
#              8767896789,
#              9899965678)
# )
data<-read.delim("d9_smoke_basin.txt", header = FALSE,  colClasses = "character")
data%>%head()

heights<-data %>%
separate(V1, into = list(paste0('pos',0:(nchar(data[1,]))))%>%unlist(), sep = "")%>%
  select(-pos0)%>%
  mutate_if(is.character,as.numeric)



#heights[row,col]
low_points<-data.frame(val = as.numeric(), pos = as.character())



check_pos_exists<-function(df_element){
  if(length(df_element)>0){var = df_element}
  else{var = 10}
  return(var)
}

for(row in 1:nrow(heights)){
  for(col in 1:ncol(heights)){
    print(paste0("row = ", row))
    print(paste0("col = ", col))
    
    up <- check_pos_exists(heights[row-1, col] )
    down<- if(row+1<=nrow(heights)) {
      print("here")
      check_pos_exists(heights[row+1, col])
    } else {
        heights[row, col]+1}
    left <- check_pos_exists(heights[row, col-1])
    right <- check_pos_exists(heights[row, col+1])
    print(paste0(up,",",down,",",left,",",right ))
    
    if(heights[row, col] <  up &
       heights[row, col] <  down &
       heights[row, col] <  left &
       heights[row, col] <  right 
    ){
      low_points <- low_points%>%rbind(data.frame(val= heights[row, col],
                                                  pos = paste0(row,",",col)) )
    }
  }
}

low_points

risk_level <- low_points$val+1
## total risk
sum(risk_level)
