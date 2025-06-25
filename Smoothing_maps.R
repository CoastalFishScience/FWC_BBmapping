###Smoothing kernel for maps then validation
library(terra)
library(tidyverse)
library(raster)
library(parallel)
library(caret)

###Binary maps
fielddat <- read.csv('S2_BB_test_set_class.csv')
head(fielddat)
fielddat <- fielddat %>% mutate(class10 = if_else(class10 == 'SAV', 2, 1))

##2023
m23 <- rast('Maps/Binary/S2_BB_SAVmap2023_cover10.tif')
plot(m23)
m23 <- as.numeric(m23)
m23_int <- round(m23)
writeRaster(m23_int, "Maps/m23_int.tif", datatype = "INT2S", overwrite = TRUE)
m23_int <- rast('Maps/m23_int.tif')
datatype(m23_int)
# Create 3x3 window
window <- matrix(1, 3, 3)
m23_smooth <- terra::focal(m23_int, w = window, fun = modal, na.rm = TRUE)
plot(m23_smooth)
fielddat2310 <- fielddat %>% dplyr::filter(Year == 2023) %>% dplyr::select(TOT, UTM.Easting, UTM.Northing, class10)
#make this a spatial object for extraction
fd2310 <- fielddat2310 %>%
  st_as_sf(coords = c("UTM.Easting", "UTM.Northing"), crs = 32617) 
fd2310e <- extract(m23_smooth, fd2310, bind = T) %>% as.data.frame() 
head(fd2310e)
cm2310 <- confusionMatrix(factor(fd2310e$class), factor(fd2310e$class10))
cm2310

# Convert to polygons (group = TRUE merges adjacent cells with same value)
m23s <- as.polygons(m23_smooth, dissolve = TRUE)  # dissolve is TRUE by default

# Export as ESRI Shapefile
writeVector(m23s, "Maps/Binary/Binary_2023.shp", filetype = "ESRI Shapefile")

##2024
m24 <- rast('Maps/Binary/S2_BB_SAVmap2024_cover10.tif')
plot(m24)
m24 <- as.numeric(m24)
m24_int <- round(m24)
writeRaster(m24_int, "Maps/m24_int.tif", datatype = "INT2S", overwrite = TRUE)
m24_int <- rast('Maps/m24_int.tif')
datatype(m24_int)
# Create 3x3 window
window <- matrix(1, 3, 3)
m24_smooth <- terra::focal(m24_int, w = window, fun = modal, na.rm = TRUE)
plot(m24_smooth)
fielddat2410 <- fielddat %>% dplyr::filter(Year == 2024) %>% dplyr::select(TOT, UTM.Easting, UTM.Northing, class10)
#make this a spatial object for extraction
fd2410 <- fielddat2410 %>%
  st_as_sf(coords = c("UTM.Easting", "UTM.Northing"), crs = 32617) 
fd2410e <- extract(m24_smooth, fd2410, bind = T) %>% as.data.frame() 
head(fd2410e)
cm2410 <- confusionMatrix(factor(fd2410e$class), factor(fd2410e$class10))
cm2410

# Convert to polygons (group = TRUE merges adjacent cells with same value)
m24s <- as.polygons(m24_smooth, dissolve = TRUE)  # dissolve is TRUE by default

# Export as ESRI Shapefile
writeVector(m24s, "Maps/Binary/Binary_2024.shp", filetype = "ESRI Shapefile")

##2025
m25 <- rast('Maps/Binary/S2_BB_SAVmap2025_cover10.tif')
plot(m25)
m25 <- as.numeric(m25)
m25_int <- round(m25)
writeRaster(m25_int, "Maps/Binary/m25_int.tif", datatype = "INT2S", overwrite = TRUE)
m25_int <- rast('Maps/Binary/m25_int.tif')
datatype(m25_int)
# Create 3x3 window
window <- matrix(1, 3, 3)
m25_smooth <- terra::focal(m25_int, w = window, fun = modal, na.rm = TRUE)
plot(m25_smooth)
fielddat2510 <- fielddat %>% dplyr::filter(Year == 2025) %>% dplyr::select(TOT, UTM.Easting, UTM.Northing, class10)
#make this a spatial object for extraction
fd2510 <- fielddat2510 %>%
  st_as_sf(coords = c("UTM.Easting", "UTM.Northing"), crs = 32617) 
fd2510e <- extract(m25_smooth, fd2510, bind = T) %>% as.data.frame() 
head(fd2510e)
cm2510 <- confusionMatrix(factor(fd2510e$class), factor(fd2510e$class10))
cm2510

# Convert to polygons (group = TRUE merges adjacent cells with same value)
m25s <- as.polygons(m25_smooth, dissolve = TRUE)  # dissolve is TRUE by default

# Export as ESRI Shapefile
writeVector(m25s, "Maps/Binary/Binary_2025.shp", filetype = "ESRI Shapefile")


##2016
m16 <- rast('Maps/Binary/S2_BB_SAVmap2016_cover10.tif')
plot(m16)
m16 <- as.numeric(m16)
m16_int <- round(m16)
writeRaster(m16_int, "Maps/m16_int.tif", datatype = "INT2S", overwrite = TRUE)
m16_int <- rast('Maps/m16_int.tif')
datatype(m16_int)
# Create 3x3 window
window <- matrix(1, 3, 3)
m16_smooth <- terra::focal(m16_int, w = window, fun = modal, na.rm = TRUE)
plot(m16_smooth)

# Convert to polygons (group = TRUE merges adjacent cells with same value)
m16s <- as.polygons(m16_smooth, dissolve = TRUE)  # dissolve is TRUE by default

# Export as ESRI Shapefile
writeVector(m16s, "Maps/Binary/Binary_2016.shp", filetype = "ESRI Shapefile")

##2017
m17 <- rast('Maps/Binary/S2_BB_SAVmap2017_cover10.tif')
plot(m17)
m17 <- as.numeric(m17)
m17_int <- round(m17)
writeRaster(m17_int, "Maps/m17_int.tif", datatype = "INT2S", overwrite = TRUE)
m17_int <- rast('Maps/m17_int.tif')
datatype(m17_int)
# Create 3x3 window
window <- matrix(1, 3, 3)
m17_smooth <- terra::focal(m17_int, w = window, fun = modal, na.rm = TRUE)
plot(m17_smooth)

# Convert to polygons (group = TRUE merges adjacent cells with same value)
m17s <- as.polygons(m17_smooth, dissolve = TRUE)  # dissolve is TRUE by default

# Export as ESRI Shapefile
writeVector(m17s, "Maps/Binary/Binary_2017.shp", filetype = "ESRI Shapefile")

##2018
m18 <- rast('Maps/Binary/S2_BB_SAVmap2018_cover10.tif')
plot(m18)
m18 <- as.numeric(m18)
m18_int <- round(m18)
writeRaster(m18_int, "Maps/m18_int.tif", datatype = "INT2S", overwrite = TRUE)
m18_int <- rast('Maps/m18_int.tif')
datatype(m18_int)
# Create 3x3 window
window <- matrix(1, 3, 3)
m18_smooth <- terra::focal(m18_int, w = window, fun = modal, na.rm = TRUE)
plot(m18_smooth)

# Convert to polygons (group = TRUE merges adjacent cells with same value)
m18s <- as.polygons(m18_smooth, dissolve = TRUE)  # dissolve is TRUE by default

# Export as ESRI Shapefile
writeVector(m18s, "Maps/Binary/Binary_2018.shp", filetype = "ESRI Shapefile")

##2019
m19 <- rast('Maps/Binary/S2_BB_SAVmap2019_cover10.tif')
plot(m19)
m19 <- as.numeric(m19)
m19_int <- round(m19)
writeRaster(m19_int, "Maps/m19_int.tif", datatype = "INT2S", overwrite = TRUE)
m19_int <- rast('Maps/m19_int.tif')
datatype(m19_int)
# Create 3x3 window
window <- matrix(1, 3, 3)
m19_smooth <- terra::focal(m19_int, w = window, fun = modal, na.rm = TRUE)
plot(m19_smooth)

# Convert to polygons (group = TRUE merges adjacent cells with same value)
m19s <- as.polygons(m19_smooth, dissolve = TRUE)  # dissolve is TRUE by default

# Export as ESRI Shapefile
writeVector(m19s, "Maps/Binary/Binary_2019.shp", filetype = "ESRI Shapefile")

##2020
m20 <- rast('Maps/Binary/S2_BB_SAVmap2020_cover10.tif')
plot(m20)
m20 <- as.numeric(m20)
m20_int <- round(m20)
writeRaster(m20_int, "Maps/m20_int.tif", datatype = "INT2S", overwrite = TRUE)
m20_int <- rast('Maps/m20_int.tif')
datatype(m20_int)
# Create 3x3 window
window <- matrix(1, 3, 3)
m20_smooth <- terra::focal(m20_int, w = window, fun = modal, na.rm = TRUE)
plot(m20_smooth)

# Convert to polygons (group = TRUE merges adjacent cells with same value)
m20s <- as.polygons(m20_smooth, dissolve = TRUE)  # dissolve is TRUE by default

# Export as ESRI Shapefile
writeVector(m20s, "Maps/Binary/Binary_2020.shp", filetype = "ESRI Shapefile")

##2021
m21 <- rast('Maps/Binary/S2_BB_SAVmap2021_cover10.tif')
plot(m21)
m21 <- as.numeric(m21)
m21_int <- round(m21)
writeRaster(m21_int, "Maps/m21_int.tif", datatype = "INT2S", overwrite = TRUE)
m21_int <- rast('Maps/m21_int.tif')
datatype(m21_int)
# Create 3x3 window
window <- matrix(1, 3, 3)
m21_smooth <- terra::focal(m21_int, w = window, fun = modal, na.rm = TRUE)
plot(m21_smooth)

# Convert to polygons (group = TRUE merges adjacent cells with same value)
m21s <- as.polygons(m21_smooth, dissolve = TRUE)  # dissolve is TRUE by default

# Export as ESRI Shapefile
writeVector(m21s, "Maps/Binary/Binary_2021.shp", filetype = "ESRI Shapefile")

##2022
m22 <- rast('Maps/Binary/S2_BB_SAVmap2022_cover10.tif')
plot(m22)
m22 <- as.numeric(m22)
m22_int <- round(m22)
writeRaster(m22_int, "Maps/m22_int.tif", datatype = "INT2S", overwrite = TRUE)
m22_int <- rast('Maps/m22_int.tif')
datatype(m22_int)
# Create 3x3 window
window <- matrix(1, 3, 3)
m22_smooth <- terra::focal(m22_int, w = window, fun = modal, na.rm = TRUE)
plot(m22_smooth)

# Convert to polygons (group = TRUE merges adjacent cells with same value)
m22s <- as.polygons(m22_smooth, dissolve = TRUE)  # dissolve is TRUE by default

# Export as ESRI Shapefile
writeVector(m22s, "Maps/Binary/Binary_2022.shp", filetype = "ESRI Shapefile")