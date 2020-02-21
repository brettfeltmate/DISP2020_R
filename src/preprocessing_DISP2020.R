setwd('/Users/Brett/Dalhousie/Klein/DISP/2020/Analysis/')

library(tidyverse)

dat <- list.files(path="Data", pattern="*.txt", full.names=TRUE) %>%
  map_df(~read.csv(., header=TRUE, sep='\t', skip=16))
    
dat$participant <- str_sub(dat$participant, 0, 6)

str(dat)
summary(dat)

dat <- filter(dat, !practicing)

groupings = c('participant', 'search_type', 'stimulus_type')

for (g in groupings) {
  dat %>% 
    group_by((!!as.symbol(g))) %>% 
      summarize(
        prop.absent = mean(response=='absent'),
        prop.present = mean(response=='present'),
        prop.none = mean(response=='none')
      ) %>% 
    print()
}
                    

dat$response <- factor(dat$response, levels=c('absent', 'present'))
dat$set_size <- factor(dat$set_size, levels = c(8,12,16))

colour.space <- filter(dat, stimulus_type == 'COLOUR' & search_type == 'space')
colour.time <- filter(dat, stimulus_type == 'COLOUR' & search_type == 'time')

line.space <- filter(dat, stimulus_type == 'LINE' & search_type == 'space')
line.time <- filter(dat, stimulus_type == 'LINE' & search_type == 'time')

save(colour.space, file="colour_space.RData")
save(colour.time, file="colour_time.RData")
save(line.space, file="line_space.RData")
save(line.time, file="line_time.RData")


