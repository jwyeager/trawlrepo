###############################################
#                                             #
#     Trawl Data Preparation and Cleaning     #
#                                             #
###############################################
library(tidyverse)
library(rfishbase)
library(magrittr)

# Import trawl catch data
tSum <- read.csv('scratch_tSumm0822.csv')
tDet <- read.csv('scratch_tDetail0822.csv')


## assign correct vector types
tSum$DATE %<>% mdy()
tSum$CODE %<>% as.character()

tDet$CODE %<>% as.character()
tDet$DATE %<>% mdy()

tSum <- tibble(tSum)
tDet <- tibble(tDet)

str(tSum)
str(tDet)
## remove empty/useless columns
drop.cols <- c('X', 'X.1', 'X.2')                      # name of columns to drop
tSum <- tSum %>% select(-one_of(drop.cols))
tDet <- tDet %>% select(-one_of(drop.cols))

str(tSum)
str(tDet)

###################################
##  Get Common Names (Somehow) 


# get list of spp caught
(tSpp <- unique(tSum$NAME))
tSpp <- na.omit(tSpp)
# fishbase_pane()   # ONLY RUN IF CONNECTION NOT OPEN ALREADY - TAKES FOREVER #

# use fishbase to validate sci. names for typos etc
vSpp <- validate_names(tSpp, server = getOption('FISHBASE_API', 'fishbase'))

dSpp <- tibble(spp.name = tSpp, val.spp = vSpp)

com.names <- common_names(species_list = tSpp,
                          server = getOption("FISHBASE_API", "fishbase"))

#############################################
##  Subset by year & get annual biomass

tSum$YEAR <- year(tSum$DATE)

tSum08 <- subset(tSum, YEAR == 2008)
tSum09 <- subset(tSum, YEAR == 2009)
tSum10 <- subset(tSum, YEAR == 2010)
tSum11 <- subset(tSum, YEAR == 2011)
tSum12 <- subset(tSum, YEAR == 2012)
tSum13 <- subset(tSum, YEAR == 2013)
tSum14 <- subset(tSum, YEAR == 2014)
tSum15 <- subset(tSum, YEAR == 2015)
tSum16 <- subset(tSum, YEAR == 2016)
tSum17 <- subset(tSum, YEAR == 2017)
tSum18 <- subset(tSum, YEAR == 2018)
tSum19 <- subset(tSum, YEAR == 2019)
tSum20 <- subset(tSum, YEAR == 2020)
tSum21 <- subset(tSum, YEAR == 2021)
tSum22 <- subset(tSum, YEAR == 2022)

## Annual catch weight
Annual.Sum <- tSum %>% 
  group_by(NAME, YEAR) %>% 
  summarise(total.weight.g = sum(as.numeric(TOT.WEIGHT), na.rm = TRUE),
            total.catch.n = sum(as.numeric(TOT.NUM), na.rm = FALSE))
