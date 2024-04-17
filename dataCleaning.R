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


# ## Annual catch weight
# Annual.Sum <- tSum %>% 
#   group_by(NAME, YEAR) %>% 
#   summarise(total.weight.g = sum(as.numeric(TOT.WEIGHT), na.rm = TRUE),
#             total.catch.n = sum(as.numeric(TOT.NUM), na.rm = FALSE))


# get trawl area and num trawl events in each year

kph <- 4.83
mps <- 1.34
trawl.duration <- 600
net.size <- 4.88

grid.cell.area.km2 <- 1.14

(trawl.dist <- trawl.duration * mps) #[1] 804
(trawl.area <- trawl.dist * net.size) #[1] 3923.52
(trawl.area.km2 <- trawl.area / (1.0 * 10^6)) #[1] 0.00392352
(trawls.per.square <- grid.cell.area.km2 / trawl.area.km2) #[1] 290.5554

  # estimated biomass per 'cell'
tSum$biom.dens.cell.kg <- (tSum$TOT.WEIGHT / 1000) / grid.cell.area.km2
tSum$biom.dens.cell.t <- (((tSum$TOT.WEIGHT / 1000) / 1000) / grid.cell.area.km2)

month.fixed.locs <- 19
n.trawl.areas <- 12 # one random sta from each area per month

# number of cells sampled in a given year
(annual.n.trawls <- 12 * (month.fixed.locs + n.trawl.areas)) #[1] 372


potential.sta <- 437 # could also be interpreted as num grid squares in study area?
(potential.area <- potential.sta * trawl.area.km2) #[1] 1.714578

# get biomass density

## Annual catch weight
Annual.Sum <- tSum %>% 
  group_by(NAME, YEAR) %>% 
  summarise(sum.biom.dens.kg = sum(as.numeric(biom.dens.cell.kg), na.rm = TRUE),
            sum.biom.dens.t = sum(as.numeric(biom.dens.cell.t), na.rm = TRUE),
            mean.biom.dens.kg = mean(as.numeric(biom.dens.cell.kg), na.rm = TRUE),
            mean.biom.dens.t = mean(as.numeric(biom.dens.cell.t), na.rm = TRUE))

Annual.Sum$biomass.kg <- Annual.Sum$mean.biom.dens.kg * annual.n.trawls
Annual.Sum$biomass.t <- Annual.Sum$mean.biom.dens.t * annual.n.trawls

# get biomass (t) with biomass density * sea surface area of cells

sound.area.km2 <- 2128.87
(tot.SA.cells <- grid.cell.area.km2 * annual.n.trawls) #[1] 424.08

(cells.in.sound <- sound.area.km2 / grid.cell.area.km2) # [1] 1867.43

Annual.Sum$est.biomass.t <- Annual.Sum$biom.density * cells.in.sound

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

# need to remove commas from catch weights
dLandings$Pounds <- as.numeric(gsub(",", "", dLandings$Pounds))
dLandings$Metric.Tons <- as.numeric(gsub(",", "", dLandings$Metric.Tons))

# get landings in tons per km2 - divide catch by area of MS sound
dLandings$t.km2 <- dLandings$Metric.Tons / sound.area.km2
dLandings
