##Organizing data for Binary mapping
#6/16/2025
#Authors: Jon Rodemann and Marianna Coppola
library(tidyverse)
library(sf)
library(terra)
library(geosphere)

#load in data
#BB data
up21 <- read.csv('upto_2021_dat.csv')
ad22 <- read.csv('ad22_23_dat.csv')
#locations
loc <- read.csv('upto_2021_coor.csv')

#combine data
dat <- rbind(up21, ad22)

head(dat)
head(loc)

loc <- loc %>% rename(Station = StationID)
head(loc)

dat_cor <- merge(dat, loc, by = "Station")
head(dat_cor)

dat_binary <- dat_cor %>% dplyr::select(Station, Year, GRID, TOT, Latitude, Longitude)
dat_class <- dat_cor %>% dplyr::select(-c(Observer, Date, Comments))

head(dat_binary)

dat_binary <- dat_binary %>% arrange(Station, Year, GRID) %>% mutate(Longitude = -(Longitude))
head(dat_binary)
write.csv(dat_binary, file = 'dat_binary.csv')

dat_class <- dat_class %>% arrange(Station, Year, GRID) %>% mutate(Longitude = -(Longitude))
write.csv(dat_class, file = 'dat_class.csv')

###Binary data transect####
# Remove GRID 2 and 3
dat_grid1 <- dat_binary %>%
  filter(GRID == 1)


# Convert to sf and transform to UTM zone 17N
dat_utm <- dat_grid1 %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(32617)  # EPSG:32617 = UTM Zone 17N

#Create function to do each of the stations

step_BB <- function(data, x, y1, y2, y3){

#Define heading and distances (in meters)
angle_deg <- x
angle_rad <- angle_deg * pi / 180
dx_1 <- y1 * cos(angle_rad)
dy_1 <- y1 * sin(angle_rad)
dx_2 <- y2 * cos(angle_rad)
dy_2 <- y2 * sin(angle_rad)
dx_3 <- y3 * cos(angle_rad)
dy_3 <- y3 * sin(angle_rad)

# Compute new points
dat_expanded <- data %>%
  mutate(
    easting = st_coordinates(.)[, 1],
    northing = st_coordinates(.)[, 2]
  ) %>%
  select(-geometry) %>%
  rowwise() %>%
  do({
    df <- .
    bind_rows(
      tibble(Station = df$Station, Year = df$Year, GRID = 1, TOT = NA,
             easting = df$easting + dx_1, northing = df$northing + dy_1),
      tibble(Station = df$Station, Year = df$Year, GRID = 2, TOT = NA,
             easting = df$easting + dx_2, northing = df$northing + dy_2),
      tibble(Station = df$Station, Year = df$Year, GRID = 3, TOT = NA,
             easting = df$easting + dx_3, northing = df$northing + dy_3)
    )
  }) %>%
  ungroup()
}

#Test out function
head(dat_utm)
unique(dat_utm$Station)
bh6 <- dat_utm %>% filter(Station == 'BH06') %>% step_BB(., 215, 36, 70, 123)
bh10 <- dat_utm %>% filter(Station == 'BH10') %>% step_BB(., 330, 0, 59, 132)
bh14 <- dat_utm %>% filter(Station == 'BH14') %>% step_BB(., 225, 50, 100, 125)
bh22 <- dat_utm %>% filter(Station == 'BH22') %>% step_BB(., 240, 30, 110, 123)
bh29 <- dat_utm %>% filter(Station == 'BH29') %>% step_BB(., 220, 10, 70, 125)
bh31 <- dat_utm %>% filter(Station == 'BH31') %>% step_BB(., 300, 35, 85, 146)
bh34 <- dat_utm %>% filter(Station == 'BH34') %>% step_BB(., 90, 35, 65, 146)
bh35 <- dat_utm %>% filter(Station == 'BH35') %>% step_BB(., 30, 0, 75, 147)
bh36 <- dat_utm %>% filter(Station == 'BH36') %>% step_BB(., 300, 29, 83, 136)
bh37 <- dat_utm %>% filter(Station == 'BH37') %>% step_BB(., 165, 13, 85, 145)
bh39 <- dat_utm %>% filter(Station == 'BH39') %>% step_BB(., 40, 63, 97, 147)

dat_all <- do.call(rbind, list(bh6, bh10, bh14, bh22, bh29, bh31, bh34, bh35, bh36, bh37, bh39))
head(dat_all)

dat_all_binary <- cbind(dat_binary, dat_all)
head(dat_all_binary)
dat_all_binary <- dat_all_binary[,c(1,2,3,4,11,12)]

write.csv(dat_all_binary, file = 'dat_all_binary.csv')

# Convert back to lat/lon for export or visualization
# dat_final <- st_as_sf(dat_all_binary, coords = c("easting", "northing"), crs = 32617) %>%
#   st_transform(4326) %>%
#   mutate(Longitude = st_coordinates(.)[,1],
#          Latitude = st_coordinates(.)[,2]) %>%
#   st_drop_geometry()

#####repeat process for class data#####
# Remove GRID 2 and 3
dat_grid2 <- dat_class %>%
  filter(GRID == 1)

# Convert to sf and transform to UTM zone 17N
dat_utm2 <- dat_grid2 %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(32617)  # EPSG:32617 = UTM Zone 17N

#function created in last step

#Test out function
head(dat_utm2)
unique(dat_utm$Station)
bh6 <- dat_utm2 %>% filter(Station == 'BH06') %>% step_BB(., 215, 36, 70, 123)
bh10 <- dat_utm2 %>% filter(Station == 'BH10') %>% step_BB(., 330, 0, 59, 132)
bh14 <- dat_utm2 %>% filter(Station == 'BH14') %>% step_BB(., 225, 50, 100, 125)
bh22 <- dat_utm2 %>% filter(Station == 'BH22') %>% step_BB(., 240, 30, 110, 123)
bh29 <- dat_utm2 %>% filter(Station == 'BH29') %>% step_BB(., 220, 10, 70, 125)
bh31 <- dat_utm2 %>% filter(Station == 'BH31') %>% step_BB(., 300, 35, 85, 146)
bh34 <- dat_utm2 %>% filter(Station == 'BH34') %>% step_BB(., 90, 35, 65, 146)
bh35 <- dat_utm2 %>% filter(Station == 'BH35') %>% step_BB(., 30, 0, 75, 147)
bh36 <- dat_utm2 %>% filter(Station == 'BH36') %>% step_BB(., 300, 29, 83, 136)
bh37 <- dat_utm2 %>% filter(Station == 'BH37') %>% step_BB(., 165, 13, 85, 145)
bh39 <- dat_utm2 %>% filter(Station == 'BH39') %>% step_BB(., 40, 63, 97, 147)

dat_all2 <- do.call(rbind, list(bh6, bh10, bh14, bh22, bh29, bh31, bh34, bh35, bh36, bh37, bh39))
head(dat_all)

dat_all_class <- cbind(dat_class, dat_all2)
head(dat_all_class)
str(dat_all_class)
dat_all_class <- dat_all_class[,-c(38, 37, 36, 35, 34, 33)]

write.csv(dat_all_class, file = 'dat_all_class.csv')

# Convert back to lat/lon for export or visualization
# dat_final <- st_as_sf(dat_all_class, coords = c("easting", "northing"), crs = 32617) %>%
#   st_transform(4326) %>%
#   mutate(Longitude = st_coordinates(.)[,1],
#          Latitude = st_coordinates(.)[,2]) %>%
#   st_drop_geometry()
