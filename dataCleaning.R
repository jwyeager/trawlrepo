###############################################
#                                             #
#     Trawl Data Preparation and Cleaning     #
#                                             #
###############################################
library(tidyverse)
library(rfishbase)

## assign correct vector types
tSumm0822$DATE %<>% mdy()
tSumm0822$CODE %<>% as.character()

tDetail0822$CODE %<>% as.character()
tDetail0822$DATE %<>% mdy()

tHydro0822$DATE %<>% mdy()
tHydro0822$STATION %<>% as.character()

## remove empty/useless columns
drop.cols <- c('X', 'X.1')  #name of columns to drop
tHydro0822 <- tHydro0822 %>% select(-one_of(drop.cols))
tSumm0822 <- tSumm0822 %>% select(-one_of(drop.cols))

###################################
#   Get Common Names (Somehow)    #
###################################
library(rfishbase)
