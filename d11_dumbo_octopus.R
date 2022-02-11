## octopuses that flash light when their energy gets to 9.
## Any octopus that flashes triggeres the neighbouring (horiz/vert/diag) to increase in value by 1
## if that makes them flash then they also trigger.
## each step increases all values by 1, then allowes all the flashing to finish, then re-sets any that have flashed to 0
library(tidyverse)
library(readr)

## test data
octo_energy<-data.frame(energy = c(5483143223,
                         2745854711,
                         5264556173,
                         6141336146,
                         6357385478,
                         4167524645,
                         2176841721,
                         6882881134,
                         4846848554,
                         5283751526))
## puzzle data
octo_energy<-data.frame(energy = c(6227618536,
                                   2368158384,
                                   5385414113,
                                   4556757523,
                                   6746486724,
                                   4881323884,
                                   4648263744,
                                   4871332872,
                                   4724128228,
                                   4316512167))

octo_energy<-octo_energy %>%
  separate(energy, into = list(paste0('pos',0:(nchar(octo_energy[1,]))))%>%unlist(), sep = "")%>%
  select(-pos0)%>%
  mutate_if(is.character,as.numeric)
octo_energy

## function find all the 9s
find_new_9s <- function(current_df, old_9s) {
  ## set empty df to find elements newly 9+
  turned_9 <-data.frame(row_num = as.numeric(), col_num = as.numeric())
  ## find all items 9+
  for (row in 1:nrow(current_df)) {
    for (col in 1:ncol(current_df)) {
      if (current_df[row, col] >= 9)
        turned_9 <-
          turned_9 %>% rbind(data.frame(row_num = row, col_num = col))
    }
  }
  #remove any items 9+ we've alread dealt with on a prior loop
  turned_9 <- anti_join(turned_9, old_9s, by = c("row_num", "col_num"))
  ## add these new 9+s to the old 9s list so we don't deal with them again.
  flashes<<-flashes+nrow(turned_9)
  old_9s<-old_9s%>%rbind(turned_9)
  all_flash<-if(nrow(old_9s)==100){TRUE} else {FALSE}
  #print(all_flash)
  return(list(turned_9,old_9s, all_flash))
}

flashes <- 0
all_flash<-FALSE
new_octo_energy<-octo_energy+1
for(step in 2:300){
  #print(step)
  #print(new_octo_energy)
  
  ## re-set data frame
  old_9s<-data.frame(row_num = as.numeric(), col_num = as.numeric())
  ## re-run 9 checker
  run_find_new_9s<-find_new_9s(new_octo_energy, old_9s) ## this returns a list of two items
  turned_9<<-run_find_new_9s[[1]]
  old_9s<<-run_find_new_9s[[2]]
  
  while(nrow(turned_9)>0){
    for(pos in 1:nrow(turned_9))
      {
        row <-turned_9$row_num[pos]
        col <-turned_9$col_num[pos]
        #print(paste0("row = ", row, ", col = ", col))
        if(row-1 != 0){new_octo_energy[row-1,col]<-new_octo_energy[row-1,col]+1 }#up
        if(row+1 != nrow(new_octo_energy)+1){new_octo_energy[row+1,col]<-new_octo_energy[row+1,col]+1 } #down
        if(col-1 != 0){new_octo_energy[row,col-1]<-new_octo_energy[row,col-1]+1 }#left
        if(col+1 != ncol(new_octo_energy)+1){new_octo_energy[row,col+1]<-new_octo_energy[row,col+1]+1 } #right
        if(row-1 != 0 & col-1 !=0){new_octo_energy[row-1,col-1]<-new_octo_energy[row-1,col-1]+1 }#up-left
        if(row-1 != 0 & col+1 !=ncol(new_octo_energy)+1){new_octo_energy[row-1,col+1]<-new_octo_energy[row-1,col+1]+1 }#up-right
        if(row+1 != nrow(new_octo_energy)+1 & col-1 !=0){new_octo_energy[row+1,col-1]<-new_octo_energy[row+1,col-1]+1 }#down-left
        if(row+1 != nrow(new_octo_energy)+1 & col+1 !=ncol(new_octo_energy)+1){new_octo_energy[row+1,col+1]<-new_octo_energy[row+1,col+1]+1 }#down-right
      }
  run_find_new_9s<-find_new_9s(new_octo_energy, old_9s)
  turned_9<<-run_find_new_9s[[1]]
  old_9s<<-run_find_new_9s[[2]]
  all_flash<<-run_find_new_9s[[3]]
  }
new_octo_energy<-new_octo_energy+1 ## add one to all elements to begin next step
new_octo_energy[new_octo_energy>9]<-0 #any elements >9  are re-set to 0

if(all_flash == TRUE){
  print(paste0("step: ", step, " - all flashed")) 
  break
  }
}

new_octo_energy
print(paste0( "there were ", flashes, " flashes"))

## part 1 - find the number of flashes after 100 steps 
## part 2 -  find the step where all 100 elements flash - 276






