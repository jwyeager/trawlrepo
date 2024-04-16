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


# # get list of spp caught
# (tSpp <- unique(tSum$NAME))
# tSpp <- na.omit(tSpp)
# # fishbase_pane()   # ONLY RUN IF CONNECTION NOT OPEN ALREADY - TAKES FOREVER #
# 
# # use fishbase to validate sci. names for typos etc
# vSpp <- validate_names(tSpp, server = getOption('FISHBASE_API', 'fishbase'))
# 
# dSpp <- tibble(spp.name = tSpp, val.spp = vSpp)
# 
# com.names <- common_names(species_list = tSpp,
#                           server = getOption("FISHBASE_API", "fishbase"))

#############################################
##  Subset by year & get annual biomass

tSum$YEAR <- year(tSum$DATE)

# mean weight of spp per trawl
tSum$MEAN.WT.G <- tSum$TOT.WEIGHT / tSum$TOT.NUM

## Annual catch weight
Annual.Sum <- tSum %>% 
  group_by(NAME, YEAR) %>% 
  summarise(total.weight.g = sum(as.numeric(TOT.WEIGHT), na.rm = TRUE),
            total.catch.n = sum(as.numeric(TOT.NUM), na.rm = FALSE))

Annual.Sum$total.weight.kg <- Annual.Sum$total.weight.g / 1000
Annual.Sum$total.weight.t <- Annual.Sum$total.weight.kg / 1000


# subset a couple of years

tSum10 <- subset(tSum, YEAR==2010)
tSum11 <- subset(tSum, YEAR==2011)
tSum12 <- subset(tSum, YEAR==2012)
tSum13 <- subset(tSum, YEAR==2013)
tSum14 <- subset(tSum, YEAR==2014)
tSum15 <- subset(tSum, YEAR==2015)
tSum16 <- subset(tSum, YEAR==2016)
tSum17 <- subset(tSum, YEAR==2017)
tSum18 <- subset(tSum, YEAR==2018)
tSum19 <- subset(tSum, YEAR==2019)
tSum20 <- subset(tSum, YEAR==2020)

# get trawl area and num trawl events in each year
kph <- 4.83
mps <- 1.34
trawl.duration <- 600
net.size <- 4.88

(trawl.dist <- trawl.duration * mps) #[1] 804
(trawl.area <- trawl.dist * net.size) #[1] 3923.52
(trawl.area.km2 <- trawl.area / (1.0 * 10^6)) #[1] 0.00392352

month.fixed.locs <- 19
n.trawl.areas <- 12 # one sta from each area per month

(annual.n.trawls <- 12 * (month.fixed.locs + n.trawl.areas)) #[1] 372

(tot.area.sampled <- trawl.area.km2 * annual.n.trawls) #[1] 1.459549

potential.sta <- 437
(potential.area <- potential.sta * trawl.area.km2) #[1] 1.714578

# get biomass by taking total annual weight (t) / rough area sampled annually (km2)

Annual.Sum$biomass.t <- Annual.Sum$total.weight.t / tot.area.sampled
Annual.Sum$biomass.g <- Annual.Sum$total.weight.g / tot.area.sampled
Annual.Sum$biomass.kg <- Annual.Sum$total.weight.kg / tot.area.sampled
Annual.Sum <- tibble(Annual.Sum)
Annual.Sum


Annual.Sum08 <- subset(Annual.Sum, YEAR == 2008)
Annual.Sum09 <- subset(Annual.Sum, YEAR == 2009)
Annual.Sum10 <- subset(Annual.Sum, YEAR == 2010)
Annual.Sum11 <- subset(Annual.Sum, YEAR == 2011)
Annual.Sum12 <- subset(Annual.Sum, YEAR == 2012)
Annual.Sum13 <- subset(Annual.Sum, YEAR == 2013)
Annual.Sum14 <- subset(Annual.Sum, YEAR == 2014)
Annual.Sum15 <- subset(Annual.Sum, YEAR == 2015)
Annual.Sum16 <- subset(Annual.Sum, YEAR == 2016)
Annual.Sum17 <- subset(Annual.Sum, YEAR == 2017)
Annual.Sum18 <- subset(Annual.Sum, YEAR == 2018)
Annual.Sum19 <- subset(Annual.Sum, YEAR == 2019)
Annual.Sum20 <- subset(Annual.Sum, YEAR == 2020)
Annual.Sum21 <- subset(Annual.Sum, YEAR == 2021)
Annual.Sum22 <- subset(Annual.Sum, YEAR == 2022)

sound.area.km2 <- 2128.87

#################################
##      Water quality data

tWQ <- read.csv('scratch_tHydro0822.csv')
str(tWQ)
tWQ$DATE %<>% mdy()
tWQ <- tWQ %>% select(-one_of(drop.cols))
tWQ$YEAR <- year(tWQ$DATE)

Annual.WQ <- tWQ %>% 
  group_by(YEAR) %>% 
  summarise(mean.sal.sur = mean(as.numeric(SAL_SUR), na.rm = TRUE),
            mean.sal.bot = mean(as.numeric(SAL_BOT), na.rm = TRUE),
            mean.temp.sur = mean(as.numeric(TMP_SUR), na.rm = TRUE),
            mean.temp.bot = mean(as.numeric(TMP_BOT), na.rm = TRUE),
            mean.oxy.sur = mean(as.numeric(OXY_SUR), na.rm = TRUE),
            mean.oxy.bot = mean(as.numeric(OXY_BOT), na.rm = TRUE))

################################
##    Commercial Landings

landings1022 <- read.csv("FOSS_landings.csv")

dLandings <- subset(landings1022, landings1022$Confidentiality != "Confidential")
dLandings %<>% tibble()
dLandings


