#d5 trying to avoid hydrothermal vents
library(tidyverse)
library(readr)


## pull in data
lines<-read_csv("d5_hydro_vents.csv")
# ### test_data
# lines<- data.frame(lines = c('0,9 -> 5,9',
#                              '8,0 -> 0,8',
#                              '9,4 -> 3,4',
#                              '2,2 -> 2,1',
#                              '7,0 -> 7,4',
#                              '6,4 -> 2,0',
#                              '0,9 -> 2,9',
#                              '3,4 -> 1,4',
#                              '0,0 -> 8,8',
#                              '5,5 -> 8,2'
# ))

## split into df with start and end positions
lines <- lines %>%
  separate(lines, into = c('start', 'end'), sep = " -> ") %>%
  separate(start, into = c('start_x', 'start_y'), sep = ",") %>%
  separate(end, into = c('end_x', 'end_y'), sep = ",") %>%
  mutate_if(is.character, as.numeric)
lines%>%head()




#################### TASK find the points where two or more vents overlap #################### 
### Task 1: ONLY use horizontal or vertical lines
#lines<-lines %>%filter(start_x == end_x | start_y == end_y)

### Task 2: use all lines including diagonal

## make the grid of locations
grid_size<-lines%>%max()
grid<-data.frame(x = 0:grid_size,
                 y = c(0:grid_size),
                 z = 0)
grid<-grid %>%
  spread(key = y, value = z)%>%
  remove_rownames %>% column_to_rownames(var="x")%>%
  mutate_all(~replace(., is.na(.), 0)) 


################ functions ################
### if the vent covers the position add 1 to that posiiton
mark_position <- function(row, col) {
  ##diagonal lines
  if (length(row) > 1 & length(col) > 1) {
    for (pos in 1:length(row)) {
      grid[row[[pos]] + 1, col[[pos]] + 1] <- (grid[row[[pos]] + 1, col[[pos]] + 1]) + 1
    }
  }
  ## horizontal or vertical lines
  else{
    for (col_pos in 1:length(col)) {
      for (row_pos in 1:length(row)) {
        grid[row[[row_pos]] + 1, col[[col_pos]] + 1] <- (grid[row[[row_pos]] + 1, col[[col_pos]] + 1]) + 1
      }
    }
  }
  return(grid)
}

### Run over the vents and mark the positions they cover
for(line in 1:nrow(lines)){
  row = as.list(lines$start_y[[line]]:lines$end_y[[line]])
  col = as.list(lines$start_x[[line]]:lines$end_x[[line]])
  grid<-mark_position(row, col)
}

write_csv(grid, "d5_part1_grid.csv", col_names = FALSE)

## how many positions have two or more crossing lines?
sum(grid >=2)






