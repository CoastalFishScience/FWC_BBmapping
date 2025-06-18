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
