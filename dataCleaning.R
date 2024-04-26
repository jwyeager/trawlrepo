###############################################
#                                             #
#     Trawl Data Preparation and Cleaning     #
#                                             #
###############################################
library(tidyverse)
library(rfishbase)
library(magrittr)
library(stringr)

# Import trawl catch data
tSum <- read.csv('scratch_tSumm0822.csv')

## assign correct vector types
tSum$DATE %<>% mdy()
tSum$CODE %<>% as.character()

tSum <- tibble(tSum)

str(tSum)

# remove empty/useless columns
#drop.cols <- c('X', 'X.1', 'X.2')                      # name of columns to drop
#tSum <- tSum %>% select(-one_of(drop.cols))

str(tSum)

#QA/QC Spp column
(l.spp <- unique(tSum$NAME))

tSum <- tSum %>% filter(!str_detect(NAME, 'YOY'))
tSum <- tSum %>% filter(!str_detect(NAME, 'sp.'))
tSum <- tSum %>% filter(!str_detect(NAME, 'No Catch'))
tSum$NAME <- gsub(" ", "_", tSum$NAME) # replace spaces with _
tSum <- tSum %>% filter(str_detect(NAME, '_')) # filter out rows w/o full sci name


#############################################
##  Subset by year & get annual biomass

tSum$YEAR <- year(tSum$DATE)
tSum$WT.kg <- tSum$TOT.WEIGHT / 1000
tSum$WT.t <- tSum$WT.kg / 1000

# get trawl area and num trawl events in each year

kph <- 4.83
mps <- 1.34
trawl.duration <- 600
net.size <- 4.88


(trawl.dist <- trawl.duration * mps) #[1] 804
(trawl.area <- trawl.dist * net.size) #[1] 3923.52 m^2
(trawl.area.km2 <- trawl.area / (1.0 * 10^6)) #[1] 0.00392352 km^2
(trawls.per.cell <- 1 / trawl.area.km2) #[1] 254.8732 trawls to cover 1 km2

n.fixed.sta <- 19
n.rand.sta <- 12 # one random sta from each area per month

# number of cells sampled in a given year
(annual.n.trawls <- 12 * (n.fixed.sta + n.rand.sta)) #[1] 372


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


Annual.Sum <- tSum %>% 
    group_by(NAME, YEAR) %>%
    summarise(u.biom.D.kg = mean(as.numeric(WT.kg)),
              u.biom.D.t = mean(as.numeric(WT.t)))

sound.area.km2 <- 2128.87

Annual.Sum$y.Biomass.kg <- Annual.Sum$u.biom.D.kg * sound.area.km2
Annual.Sum$y.Biomass.t <- Annual.Sum$u.biom.D.t * sound.area.km2


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


model.spp <- c("Anchoa_mitchilli", "Anchoa_hepsetus", "Anchoa_lyolepis",
               "Ariopsis_felis", "Bagre_marinus", "Archosargus_probatocephalus",
               "Pogonias_cromis",
               "Callinectes_sapidus", "Callinectes_similis", "Cynoscion_nebulosus",
               "Cynoscion_arenarius", "Cynoscion_nothus", "Caranx_hippos",
               "Sciaenops_ocellatus", "Sphyrna_tiburo", "Harengula_jaguana",
               "Sardinella_janeiro", "Scomberomorus_maculatus", "Paralichthys_lethostigma",
               "Micropogonias_undulatus", "Litopenaeus_setiferus", 
               "Farfantepenaeus_aztecus", "Farfantepenaeus_duorarum",
               "Dasyatis_sabina", "Dasyatis_say", "Menticirrhus_americanus",
               "Brevoortia_patronus")

model08 <- Annual.Sum08 %>% filter(NAME %in% model.spp)
model09 <- Annual.Sum09 %>% filter(NAME %in% model.spp)
model10 <- Annual.Sum10 %>% filter(NAME %in% model.spp)
model11 <- Annual.Sum11 %>% filter(NAME %in% model.spp)
model12 <- Annual.Sum12 %>% filter(NAME %in% model.spp)
model13 <- Annual.Sum13 %>% filter(NAME %in% model.spp)
model14 <- Annual.Sum14 %>% filter(NAME %in% model.spp)
model15 <- Annual.Sum15 %>% filter(NAME %in% model.spp)
model16 <- Annual.Sum16 %>% filter(NAME %in% model.spp)
model17 <- Annual.Sum17 %>% filter(NAME %in% model.spp)
model18 <- Annual.Sum18 %>% filter(NAME %in% model.spp)
model19 <- Annual.Sum19 %>% filter(NAME %in% model.spp)
model20 <- Annual.Sum20 %>% filter(NAME %in% model.spp)
model21 <- Annual.Sum21 %>% filter(NAME %in% model.spp)

write.csv(model08, file='model08.csv')
write.csv(model09, file='model09.csv')

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
