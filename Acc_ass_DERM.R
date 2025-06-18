##Accuracy assessment - DERM data
#6/18/2025
#Authors: Jon Rodemann and Marianna Coppola

library(tidyverse)
library(terra)
library(sf)
library(caret)

###Binary####
##Load in data
SAVBB <- read.csv('dat_all_binary.csv')
head(SAVBB)
#2016 with treshold at 10% cover
map16 <- rast('Maps/S2_BB_SAVmap2016_cover10.tif')
plot(map16)
#have to classify everything at a 2 and above as dense (2)
bb16 <- SAVBB %>% dplyr::filter(Year == 2016) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 1, 1, 2))
head(bb16)
#make this a spatial object for extraction
bb16utm <- bb16 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map16)
crs(bb16utm)

bb16dat <- extract(map16, bb16utm, bind = T) %>% as.data.frame() 
head(bb16dat)
cm16 <- confusionMatrix(factor(bb16dat$class), factor(bb16dat$class1))
cm16

#2017 with treshold at 10% cover
map17 <- rast('Maps/S2_BB_SAVmap2017_cover10.tif')
plot(map17)
#have to classify everything at a 2 and above as dense (2)
bb17 <- SAVBB %>% dplyr::filter(Year == 2017) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 1, 1, 2))
head(bb17)
#make this a spatial object for extraction
bb17utm <- bb17 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map17)
crs(bb17utm)

bb17dat <- extract(map17, bb17utm, bind = T) %>% as.data.frame() 
head(bb17dat)
cm17 <- confusionMatrix(factor(bb17dat$class), factor(bb17dat$class1))
cm17

#2018 with treshold at 10% cover
map18 <- rast('Maps/S2_BB_SAVmap2018_cover10.tif')
plot(map18)
#have to classify everything at a 2 and above as dense (2)
bb18 <- SAVBB %>% dplyr::filter(Year == 2018) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 1, 1, 2))
head(bb18)
#make this a spatial object for extraction
bb18utm <- bb18 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map18)
crs(bb18utm)

bb18dat <- extract(map18, bb18utm, bind = T) %>% as.data.frame() 
head(bb18dat)
cm18 <- confusionMatrix(factor(bb18dat$class), factor(bb18dat$class1))
cm18

#2019 with treshold at 10% cover
map19 <- rast('Maps/S2_BB_SAVmap2019_cover10.tif')
plot(map19)
#have to classify everything at a 2 and above as dense (2)
bb19 <- SAVBB %>% dplyr::filter(Year == 2019) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 1, 1, 2))
head(bb19)
#make this a spatial object for extraction
bb19utm <- bb19 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map19)
crs(bb19utm)

bb19dat <- extract(map19, bb19utm, bind = T) %>% as.data.frame() 
head(bb19dat)
cm19 <- confusionMatrix(factor(bb19dat$class), factor(bb19dat$class1))
cm19

#2020 with treshold at 10% cover
map20 <- rast('Maps/S2_BB_SAVmap2020_cover10.tif')
plot(map20)
#have to classify everything at a 2 and above as dense (2)
bb20 <- SAVBB %>% dplyr::filter(Year == 2020) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 1, 1, 2))
head(bb20)
#make this a spatial object for extraction
bb20utm <- bb20 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map20)
crs(bb20utm)

bb20dat <- extract(map20, bb20utm, bind = T) %>% as.data.frame() 
head(bb20dat)
cm20 <- confusionMatrix(factor(bb20dat$class), factor(bb20dat$class1))
cm20

#2021 with treshold at 10% cover
map21 <- rast('Maps/S2_BB_SAVmap2021_cover10.tif')
plot(map21)
#have to classify everything at a 2 and above as dense (2)
bb21 <- SAVBB %>% dplyr::filter(Year == 2021) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 1, 1, 2))
head(bb21)
#make this a spatial object for extraction
bb21utm <- bb21 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map21)
crs(bb21utm)

bb21dat <- extract(map21, bb21utm, bind = T) %>% as.data.frame() 
head(bb21dat)
cm21 <- confusionMatrix(factor(bb21dat$class), factor(bb21dat$class1))
cm21

#2022 with treshold at 10% cover
map22 <- rast('Maps/S2_BB_SAVmap2022_cover10.tif')
plot(map22)
#have to classify everything at a 2 and above as dense (2)
bb22 <- SAVBB %>% dplyr::filter(Year == 2022) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 1, 1, 2))
head(bb22)
#make this a spatial object for extraction
bb22utm <- bb22 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map22)
crs(bb22utm)

bb22dat <- extract(map22, bb22utm, bind = T) %>% as.data.frame() 
head(bb22dat)
cm22 <- confusionMatrix(factor(bb22dat$class), factor(bb22dat$class1))
cm22

#2023 with treshold at 10% cover
map23 <- rast('Maps/S2_BB_SAVmap2023_cover10.tif')
plot(map23)
#have to classify everything at a 2 and above as dense (2)
bb23 <- SAVBB %>% dplyr::filter(Year == 2023) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 1, 1, 2))
head(bb23)
#make this a spatial object for extraction
bb23utm <- bb23 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map23)
crs(bb23utm)

bb23dat <- extract(map23, bb23utm, bind = T) %>% as.data.frame() 
head(bb23dat)
all_levels <- union(unique(bb23dat$class), unique(bb23dat$class1))
cm23 <- confusionMatrix(factor(bb23dat$class, levels = all_levels), factor(bb23dat$class1, levels = all_levels))
cm23

###20% cover####
#2016 with treshold at 20% cover
map1620 <- rast('Maps/S2_BB_SAVmap2016_cover20.tif')
plot(map1620)
#have to classify everything at a 2 and above as dense (2)
bb1620 <- SAVBB %>% dplyr::filter(Year == 2016) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb1620)
#make this a spatial object for extraction
bb16utm20 <- bb1620 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map16)
crs(bb16utm)

bb16dat20 <- extract(map1620, bb16utm20, bind = T) %>% as.data.frame() 
head(bb16dat)
cm1620 <- confusionMatrix(factor(bb16dat20$class), factor(bb16dat20$class1))
cm16
cm1620

#2017 with treshold at 20% cover
map1720 <- rast('Maps/S2_BB_SAVmap2017_cover20.tif')
plot(map1720)
#have to classify everything at a 2 and above as dense (2)
bb1720 <- SAVBB %>% dplyr::filter(Year == 2017) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb1720)
#make this a spatial object for extraction
bb17utm20 <- bb1720 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map17)
crs(bb17utm)

bb17dat20 <- extract(map1720, bb17utm20, bind = T) %>% as.data.frame() 
head(bb17dat)
cm1720 <- confusionMatrix(factor(bb17dat20$class), factor(bb17dat20$class1))
cm17
cm1720

#2018 with treshold at 20% cover
map1820 <- rast('Maps/S2_BB_SAVmap2018_cover20.tif')
plot(map1820)
#have to classify everything at a 2 and above as dense (2)
bb1820 <- SAVBB %>% dplyr::filter(Year == 2018) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb1820)
#make this a spatial object for extraction
bb18utm20 <- bb1820 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map18)
crs(bb18utm)

bb18dat20 <- extract(map1820, bb18utm20, bind = T) %>% as.data.frame() 
head(bb18dat)
cm1820 <- confusionMatrix(factor(bb18dat20$class), factor(bb18dat20$class1))
cm18
cm1820

#2019 with treshold at 20% cover
map1920 <- rast('Maps/S2_BB_SAVmap2019_cover20.tif')
plot(map1920)
#have to classify everything at a 2 and above as dense (2)
bb1920 <- SAVBB %>% dplyr::filter(Year == 2019) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb1920)
#make this a spatial object for extraction
bb19utm20 <- bb1920 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map19)
crs(bb19utm)

bb19dat20 <- extract(map1920, bb19utm20, bind = T) %>% as.data.frame() 
head(bb19dat)
cm1920 <- confusionMatrix(factor(bb19dat20$class), factor(bb19dat20$class1))
cm19
cm1920

#2020 with treshold at 20% cover
map2020 <- rast('Maps/S2_BB_SAVmap2020_cover20.tif')
plot(map2020)
#have to classify everything at a 2 and above as dense (2)
bb2020 <- SAVBB %>% dplyr::filter(Year == 2020) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb2020)
#make this a spatial object for extraction
bb20utm20 <- bb2020 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map20)
crs(bb20utm)

bb20dat20 <- extract(map2020, bb20utm20, bind = T) %>% as.data.frame() 
head(bb20dat)
cm2020 <- confusionMatrix(factor(bb20dat20$class), factor(bb20dat20$class1))
cm20
cm2020

#2021 with treshold at 20% cover
map2120 <- rast('Maps/S2_BB_SAVmap2021_cover20.tif')
plot(map2120)
#have to classify everything at a 2 and above as dense (2)
bb2120 <- SAVBB %>% dplyr::filter(Year == 2021) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb2120)
#make this a spatial object for extraction
bb21utm20 <- bb2120 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map21)
crs(bb21utm)

bb21dat20 <- extract(map2120, bb21utm20, bind = T) %>% as.data.frame() 
head(bb21dat)
cm2120 <- confusionMatrix(factor(bb21dat20$class), factor(bb21dat20$class1))
cm21
cm2120

#2022 with treshold at 20% cover
map2220 <- rast('Maps/S2_BB_SAVmap2022_cover20.tif')
plot(map2220)
#have to classify everything at a 2 and above as dense (2)
bb2220 <- SAVBB %>% dplyr::filter(Year == 2022) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb2220)
#make this a spatial object for extraction
bb22utm20 <- bb2220 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map22)
crs(bb22utm)

bb22dat20 <- extract(map2220, bb22utm20, bind = T) %>% as.data.frame() 
head(bb22dat)
cm2220 <- confusionMatrix(factor(bb22dat20$class), factor(bb22dat20$class1))
cm22
cm2220

#2023 with treshold at 20% cover
map2320 <- rast('Maps/S2_BB_SAVmap2023_cover20.tif')
plot(map2320)
#have to classify everything at a 2 and above as dense (2)
bb2320 <- SAVBB %>% dplyr::filter(Year == 2023) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb2320)
#make this a spatial object for extraction
bb23utm20 <- bb2320 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map23)
crs(bb23utm)

bb23dat20 <- extract(map2320, bb23utm20, bind = T) %>% as.data.frame() 
head(bb23dat)
cm2320 <- confusionMatrix(factor(bb23dat20$class), factor(bb23dat20$class1))
cm23
cm2320

####30% cover######
#2016 with treshold at 30% cover
map1630 <- rast('Maps/S2_BB_SAVmap2016_cover30.tif')
plot(map1630)
#have to classify everything at a 2 and above as dense (2)
bb1630 <- SAVBB %>% dplyr::filter(Year == 2016) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb1630)
#make this a spatial object for extraction
bb16utm30 <- bb1630 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map1630)
crs(bb16utm30)

bb16dat30 <- extract(map1630, bb16utm30, bind = T) %>% as.data.frame() 
head(bb16dat30)
cm1630 <- confusionMatrix(factor(bb16dat30$class), factor(bb16dat30$class1))
cm16
cm1630

#2017 with treshold at 30% cover
map1730 <- rast('Maps/S2_BB_SAVmap2017_cover30.tif')
plot(map1730)
#have to classify everything at a 2 and above as dense (2)
bb1730 <- SAVBB %>% dplyr::filter(Year == 2017) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb1730)
#make this a spatial object for extraction
bb17utm30 <- bb1730 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map1730)
crs(bb17utm30)

bb17dat30 <- extract(map1730, bb17utm30, bind = T) %>% as.data.frame() 
head(bb17dat30)
cm1730 <- confusionMatrix(factor(bb17dat30$class), factor(bb17dat30$class1))
cm17
cm1730

#2018 with treshold at 30% cover
map1830 <- rast('Maps/S2_BB_SAVmap2018_cover30.tif')
plot(map1830)
#have to classify everything at a 2 and above as dense (2)
bb1830 <- SAVBB %>% dplyr::filter(Year == 2018) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb1830)
#make this a spatial object for extraction
bb18utm30 <- bb1830 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map1830)
crs(bb18utm30)

bb18dat30 <- extract(map1830, bb18utm30, bind = T) %>% as.data.frame() 
head(bb18dat30)
cm1830 <- confusionMatrix(factor(bb18dat30$class), factor(bb18dat30$class1))
cm18
cm1830

#2019 with treshold at 30% cover
map1930 <- rast('Maps/S2_BB_SAVmap2019_cover30.tif')
plot(map1930)
#have to classify everything at a 2 and above as dense (2)
bb1930 <- SAVBB %>% dplyr::filter(Year == 2019) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb1930)
#make this a spatial object for extraction
bb19utm30 <- bb1930 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map1930)
crs(bb19utm30)

bb19dat30 <- extract(map1930, bb19utm30, bind = T) %>% as.data.frame() 
head(bb19dat30)
cm1930 <- confusionMatrix(factor(bb19dat30$class), factor(bb19dat30$class1))
cm19
cm1930

#2020 with treshold at 30% cover
map2030 <- rast('Maps/S2_BB_SAVmap2020_cover30.tif')
plot(map2030)
#have to classify everything at a 2 and above as dense (2)
bb2030 <- SAVBB %>% dplyr::filter(Year == 2020) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb2030)
#make this a spatial object for extraction
bb20utm30 <- bb2030 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map2030)
crs(bb20utm30)

bb20dat30 <- extract(map2030, bb20utm30, bind = T) %>% as.data.frame() 
head(bb20dat30)
cm2030 <- confusionMatrix(factor(bb20dat30$class), factor(bb20dat30$class1))
cm20
cm2030

#2021 with treshold at 30% cover
map2130 <- rast('Maps/S2_BB_SAVmap2021_cover30.tif')
plot(map2130)
#have to classify everything at a 2 and above as dense (2)
bb2130 <- SAVBB %>% dplyr::filter(Year == 2021) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb2130)
#make this a spatial object for extraction
bb21utm30 <- bb2130 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map2130)
crs(bb21utm30)

bb21dat30 <- extract(map2130, bb21utm30, bind = T) %>% as.data.frame() 
head(bb21dat30)
cm2130 <- confusionMatrix(factor(bb21dat30$class), factor(bb21dat30$class1))
cm21
cm2130

#2022 with treshold at 30% cover
map2230 <- rast('Maps/S2_BB_SAVmap2022_cover30.tif')
plot(map2230)
#have to classify everything at a 2 and above as dense (2)
bb2230 <- SAVBB %>% dplyr::filter(Year == 2022) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb2230)
#make this a spatial object for extraction
bb22utm30 <- bb2230 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map2230)
crs(bb22utm30)

bb22dat30 <- extract(map2230, bb22utm30, bind = T) %>% as.data.frame() 
head(bb22dat30)
cm2230 <- confusionMatrix(factor(bb22dat30$class), factor(bb22dat30$class1))
cm22
cm2230

#2023 with treshold at 30% cover
map2330 <- rast('Maps/S2_BB_SAVmap2023_cover30.tif')
plot(map2330)
#have to classify everything at a 2 and above as dense (2)
bb2330 <- SAVBB %>% dplyr::filter(Year == 2023) %>% mutate(TOT = as.numeric(TOT)) %>% mutate(class1 = if_else(TOT <= 2, 1, 2))
head(bb2330)
#make this a spatial object for extraction
bb23utm30 <- bb2330 %>%
  st_as_sf(coords = c("easting", "northing"), crs = 32617) 

crs(map2330)
crs(bb23utm30)

bb23dat30 <- extract(map2330, bb23utm30, bind = T) %>% as.data.frame() 
head(bb23dat30)
cm2330 <- confusionMatrix(factor(bb23dat30$class), factor(bb23dat30$class1))
cm23
cm2330

#Acc ass with current day data
fielddat <- read.csv('S2_BB_test_set_class.csv')
fielddat <- fielddat %>% mutate(class10 = if_else(class10 == 'SAV', 2, 1), class20 = if_else(class20 == 'SAV', 2, 1), class30 = if_else(class30 == 'SAV', 2, 1))
head(fielddat)

##2023 with 10%
fielddat2310 <- fielddat %>% dplyr::filter(Year == 2023) %>% dplyr::select(TOT, UTM.Easting, UTM.Northing, class10)
#make this a spatial object for extraction
fd2310 <- fielddat2310 %>%
  st_as_sf(coords = c("UTM.Easting", "UTM.Northing"), crs = 32617) 
fd2310e <- extract(map23, fd2310, bind = T) %>% as.data.frame() 
cm2310 <- confusionMatrix(factor(fd2310e$class), factor(fd2310e$class10))
cm2310

##2024 with 10%
map24 <- rast('Maps/S2_BB_SAVmap2024_cover10.tif')
fielddat2410 <- fielddat %>% dplyr::filter(Year == 2024) %>% dplyr::select(TOT, UTM.Easting, UTM.Northing, class10)
#make this a spatial object for extraction
fd2410 <- fielddat2410 %>%
  st_as_sf(coords = c("UTM.Easting", "UTM.Northing"), crs = 32617) 
fd2410e <- extract(map24, fd2410, bind = T) %>% as.data.frame() 
cm2410 <- confusionMatrix(factor(fd2410e$class), factor(fd2410e$class10))
cm2410

##2025 with 10%
map25 <- rast('Maps/S2_BB_SAVmap2025_cover10.tif')
fielddat2510 <- fielddat %>% dplyr::filter(Year == 2025) %>% dplyr::select(TOT, UTM.Easting, UTM.Northing, class10)
#make this a spatial object for extraction
fd2510 <- fielddat2510 %>%
  st_as_sf(coords = c("UTM.Easting", "UTM.Northing"), crs = 32617) 
fd2510e <- extract(map25, fd2510, bind = T) %>% as.data.frame() 
cm2510 <- confusionMatrix(factor(fd2510e$class), factor(fd2510e$class10))
cm2510

##2023 with 20%
fielddat2320 <- fielddat %>% dplyr::filter(Year == 2023) %>% dplyr::select(TOT, UTM.Easting, UTM.Northing, class20)
#make this a spatial object for extraction
fd2320 <- fielddat2320 %>%
  st_as_sf(coords = c("UTM.Easting", "UTM.Northing"), crs = 32617) 
fd2320e <- extract(map2320, fd2320, bind = T) %>% as.data.frame() 
cm2320 <- confusionMatrix(factor(fd2320e$class), factor(fd2320e$class20))
cm2320

##2024 with 20%
map2420 <- rast('Maps/S2_BB_SAVmap2024_cover20.tif')
fielddat2420 <- fielddat %>% dplyr::filter(Year == 2024) %>% dplyr::select(TOT, UTM.Easting, UTM.Northing, class20)
#make this a spatial object for extraction
fd2420 <- fielddat2420 %>%
  st_as_sf(coords = c("UTM.Easting", "UTM.Northing"), crs = 32617) 
fd2420e <- extract(map2420, fd2420, bind = T) %>% as.data.frame() 
cm2420 <- confusionMatrix(factor(fd2420e$class), factor(fd2420e$class20))
cm2420

##2025 with 20%
map2520 <- rast('Maps/S2_BB_SAVmap2025_cover20.tif')
fielddat2520 <- fielddat %>% dplyr::filter(Year == 2025) %>% dplyr::select(TOT, UTM.Easting, UTM.Northing, class20)
#make this a spatial object for extraction
fd2520 <- fielddat2520 %>%
  st_as_sf(coords = c("UTM.Easting", "UTM.Northing"), crs = 32617) 
fd2520e <- extract(map2520, fd2520, bind = T) %>% as.data.frame() 
cm2520 <- confusionMatrix(factor(fd2520e$class), factor(fd2520e$class20))
cm2520

##2023 with 30%
fielddat2330 <- fielddat %>% dplyr::filter(Year == 2023) %>% dplyr::select(TOT, UTM.Easting, UTM.Northing, class30)
#make this a spatial object for extraction
fd2330 <- fielddat2330 %>%
  st_as_sf(coords = c("UTM.Easting", "UTM.Northing"), crs = 32617) 
fd2330e <- extract(map2330, fd2330, bind = T) %>% as.data.frame() 
cm2330 <- confusionMatrix(factor(fd2330e$class), factor(fd2330e$class30))
cm2310$overall
cm2320$overall
cm2330$overall

##2024 with 30%
map2430 <- rast('Maps/S2_BB_SAVmap2024_cover30.tif')
fielddat2430 <- fielddat %>% dplyr::filter(Year == 2024) %>% dplyr::select(TOT, UTM.Easting, UTM.Northing, class30)
#make this a spatial object for extraction
fd2430 <- fielddat2430 %>%
  st_as_sf(coords = c("UTM.Easting", "UTM.Northing"), crs = 32617) 
fd2430e <- extract(map2430, fd2430, bind = T) %>% as.data.frame() 
cm2430 <- confusionMatrix(factor(fd2430e$class), factor(fd2430e$class30))
cm2410$overall
cm2420$overall
cm2430$overall

##2025 with 30%
map2530 <- rast('Maps/S2_BB_SAVmap2025_cover30.tif')
fielddat2530 <- fielddat %>% dplyr::filter(Year == 2025) %>% dplyr::select(TOT, UTM.Easting, UTM.Northing, class30)
#make this a spatial object for extraction
fd2530 <- fielddat2530 %>%
  st_as_sf(coords = c("UTM.Easting", "UTM.Northing"), crs = 32617) 
fd2530e <- extract(map2530, fd2530, bind = T) %>% as.data.frame() 
cm2530 <- confusionMatrix(factor(fd2530e$class), factor(fd2530e$class30))
cm2510$overall
cm2520$overall
cm2530$overall
