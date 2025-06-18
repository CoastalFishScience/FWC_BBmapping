##Spectral mixing, clustering, etc. for multi-class map
##6/18/2025
#Authors: Jon Rodemann and Marianna Coppola

library(tidyverse)

#Load in data - let's explore all of the data for now
datall <- read.csv('S2_BB_all_spectral_data.csv')
head(datall)

#keep only the columns you want to use for clustering
datclust <- datall[,5:15]
head(datclust)

#scale data
datcs <- scale(datclust)

#set number of clusters
k <- 6

#run k-means
set.seed(630)

kmeans_result <- kmeans(datcs, centers = k, nstart = 25)
print(kmeans_result)

datall$cluster <- kmeans_result$cluster

plot(datcs, col = kmeans_result$cluster, pch = 20, cex = 2)
points(kmeans_result$centers, col = 1:k, pch = 4, cex = 4, lwd = 4)

##PCA visualization
pca <- prcomp(datcs, center = TRUE, scale. = TRUE)

# Take first two principal components
pca_data <- data.frame(pca$x[, 1:2])
pca_data$cluster <- as.factor(kmeans_result$cluster)

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means clustering visualized in PCA space") +
  theme_classic()

#investigate data based on k-means
head(datall)

datall$cluster <- as.factor(datall$cluster)

# Select variables of interest
vars <- c("TOT", "T_DR", "TSG", "Tt", "Sf", "Hw", "TMA")
str(datall)
datall$TOT <- as.numeric(datall$TOT)
datall$T_DR <- as.numeric(datall$T_DR)
datall$TSG <- as.numeric(datall$TSG)
datall$Tt <- as.numeric(datall$Tt)
datall$Sf <- as.numeric(datall$Sf)
datall$Hw <- as.numeric(datall$Hw)

datall <- datall %>%  mutate(across(all_of(vars), ~ replace_na(.x, 0)))
write.csv(datall, 'viz_dat.csv')

# Compute means# Compute meansTt
cluster_means <- datall %>%
  group_by(cluster) %>%
  summarise(across(all_of(vars), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

#plot histograms
for (var in vars) {
  p <- ggplot(datall, aes_string(x = var)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "black") +
    facet_wrap(~ cluster, scales = "free_y") +
    labs(title = paste("Density of", var, "by Cluster")) +
    theme_minimal()
  
  print(p)
}

#let's look at suggested number of K code
#elbow method
wss <- vector()

for (k in 1:10) {
  set.seed(123)
  km <- kmeans(datcs, centers = k, nstart = 25)
  wss[k] <- km$tot.withinss  # total within-cluster sum of squares
}

# Plot Elbow curve
plot(1:10, wss, type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method")

library(cluster)

silhouette_scores <- vector()

for (k in 2:10) {
  set.seed(630)
  km <- kmeans(datcs, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(datcs))
  silhouette_scores[k] <- mean(sil[, 3])  # average silhouette width
}

# Plot silhouette scores
plot(2:10, silhouette_scores[2:10], type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Width",
     main = "Silhouette Method")

library(factoextra)

set.seed(630)
gap_stat <- clusGap(datcs, FUN = kmeans, nstart = 25, K.max = 15, B = 100)

# Plot the gap statistic
fviz_gap_stat(gap_stat)

#That is not really working. Let's do some supervised investigating. I am doing TOT
#First we need to group then get spectral profile. First is 3 groups
tot3 <- function(dat, x, y){
  data <- dat %>% mutate(class = ifelse(TOT < x, 'Low',
                                        ifelse(TOT >= x & TOT < y, 'Medium', 
                                               ifelse(TOT >= y, 'High', 'NA'))))
  
  return(data)
}

#10% and 25%
t1025 <- tot3(datall, 10, 25)

TSet_long <- t1025  %>%
  pivot_longer(cols = blue:DII_green_red, names_to = "band", values_to = "reflectance")
head(TSet_long)
#TSet_long <- TSet_long[-1]
colnames(TSet_long)

band_order <- c("blue", "green", "red", "vnir", 'NDAVI', 'ind24', 'NDWI', 'WAVI', 'DII_blue_green', 'DII_blue_red', 'DII_green_red') 
TSet_long$band <- factor(TSet_long$band, levels = band_order) #bands are ordered along the EM spectrum

class_stats <- TSet_long %>%
  group_by(class, band) %>%
  summarise(
    MeanSpectralValue = mean(reflectance, na.rm = TRUE),
    SD = sd(reflectance, na.rm = TRUE),
    .groups = "drop"
  )

# Plot with standard deviation as shaded area
ggplot(class_stats, aes(x = band, y = MeanSpectralValue, color = class, group = class)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MeanSpectralValue - SD, ymax = MeanSpectralValue + SD), width = 0.2) +
  labs(title = "Total cover with 10% and 25% thresholds",
       x = "Band",
       y = "Mean Surface Reflectance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"))

ggsave('TOT1025.png')

#20% and 50%
t2050 <- tot3(datall, 20, 50)

TSet_long <- t2050  %>%
  pivot_longer(cols = blue:DII_green_red, names_to = "band", values_to = "reflectance")
head(TSet_long)
#TSet_long <- TSet_long[-1]
colnames(TSet_long)

band_order <- c("blue", "green", "red", "vnir", 'NDAVI', 'ind24', 'NDWI', 'WAVI', 'DII_blue_green', 'DII_blue_red', 'DII_green_red') 
TSet_long$band <- factor(TSet_long$band, levels = band_order) #bands are ordered along the EM spectrum

class_stats <- TSet_long %>%
  group_by(class, band) %>%
  summarise(
    MeanSpectralValue = mean(reflectance, na.rm = TRUE),
    SD = sd(reflectance, na.rm = TRUE),
    .groups = "drop"
  )

# Plot with standard deviation as shaded area
ggplot(class_stats, aes(x = band, y = MeanSpectralValue, color = class, group = class)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MeanSpectralValue - SD, ymax = MeanSpectralValue + SD), width = 0.2) +
  labs(title = "Total cover with 20% and 50% thresholds",
       x = "Band",
       y = "Mean Surface Reflectance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"))
ggsave('TOT2050.png')
#this one is really good actually. We can try this if there are differences in species.
#Next: attempt 5 classes. That is what we said we would get to FWC. Use the breakpoints for BB
tot5 <- function(dat, x, y, z, d){
  data <- dat %>% mutate(class = ifelse(TOT < x, 'Low',
                                        ifelse(TOT >= x & TOT < y, 'Medium Low', 
                                               ifelse(TOT >= y & TOT < z, 'Medium', 
                                                      ifelse(TOT >= z & TOT < d, 'Medium High',
                                                             ifelse(TOT >= d, 'High', 'NA'))))))
  
  return(data)
}

#TOT5:25:50:75
tot575 <- tot5(datall, 5, 25, 50, 75)

TSet_long <- tot575  %>%
  pivot_longer(cols = blue:DII_green_red, names_to = "band", values_to = "reflectance")
head(TSet_long)
#TSet_long <- TSet_long[-1]
colnames(TSet_long)

band_order <- c("blue", "green", "red", "vnir", 'NDAVI', 'ind24', 'NDWI', 'WAVI', 'DII_blue_green', 'DII_blue_red', 'DII_green_red') 
TSet_long$band <- factor(TSet_long$band, levels = band_order) #bands are ordered along the EM spectrum

class_stats <- TSet_long %>%
  group_by(class, band) %>%
  summarise(
    MeanSpectralValue = mean(reflectance, na.rm = TRUE),
    SD = sd(reflectance, na.rm = TRUE),
    .groups = "drop"
  )

# Plot with standard deviation as shaded area
ggplot(class_stats, aes(x = band, y = MeanSpectralValue, color = class, group = class)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MeanSpectralValue - SD, ymax = MeanSpectralValue + SD), width = 0.2) +
  labs(title = "Total cover with 20% and 50% thresholds",
       x = "Band",
       y = "Mean Surface Reflectance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"))
ggsave("tot5255075.png")
#nope. Let's try in 20 up to 50 in 10s
tot2050 <- tot5(datall, 20, 30, 40, 50)

TSet_long <- tot2050  %>%
  pivot_longer(cols = blue:DII_green_red, names_to = "band", values_to = "reflectance")
head(TSet_long)
#TSet_long <- TSet_long[-1]
colnames(TSet_long)

band_order <- c("blue", "green", "red", "vnir", 'NDAVI', 'ind24', 'NDWI', 'WAVI', 'DII_blue_green', 'DII_blue_red', 'DII_green_red') 
TSet_long$band <- factor(TSet_long$band, levels = band_order) #bands are ordered along the EM spectrum

class_stats <- TSet_long %>%
  group_by(class, band) %>%
  summarise(
    MeanSpectralValue = mean(reflectance, na.rm = TRUE),
    SD = sd(reflectance, na.rm = TRUE),
    .groups = "drop"
  )

# Plot with standard deviation as shaded area
ggplot(class_stats, aes(x = band, y = MeanSpectralValue, color = class, group = class)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MeanSpectralValue - SD, ymax = MeanSpectralValue + SD), width = 0.2) +
  labs(title = "Total cover with 20%, 30%, 40%, 50% thresholds",
       x = "Band",
       y = "Mean Surface Reflectance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"))
ggsave('TOT20304050.png')
#Getting there. Let's do 15% and go up by 15s
tot1560 <- tot5(datall, 15, 20, 45, 60)

TSet_long <- tot1560  %>%
  pivot_longer(cols = blue:DII_green_red, names_to = "band", values_to = "reflectance")
head(TSet_long)
#TSet_long <- TSet_long[-1]
colnames(TSet_long)

band_order <- c("blue", "green", "red", "vnir", 'NDAVI', 'ind24', 'NDWI', 'WAVI', 'DII_blue_green', 'DII_blue_red', 'DII_green_red') 
TSet_long$band <- factor(TSet_long$band, levels = band_order) #bands are ordered along the EM spectrum

class_stats <- TSet_long %>%
  group_by(class, band) %>%
  summarise(
    MeanSpectralValue = mean(reflectance, na.rm = TRUE),
    SD = sd(reflectance, na.rm = TRUE),
    .groups = "drop"
  )

# Plot with standard deviation as shaded area
ggplot(class_stats, aes(x = band, y = MeanSpectralValue, color = class, group = class)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MeanSpectralValue - SD, ymax = MeanSpectralValue + SD), width = 0.2) +
  labs(title = "Total cover with 15%, 30%, 45%, 60% thresholds",
       x = "Band",
       y = "Mean Surface Reflectance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"))

#Try with incorporating species
datsgma <- datall %>%
  mutate(class = case_when(
    TOT < 20 & TSG > TMA ~ "Low SG",
    TOT < 20 & TMA > TSG ~ "Low MA",
    TOT >= 20 & TOT < 50 & TSG > TMA ~ "Medium SG",
    TOT >= 20 & TOT < 50 & TMA > TSG ~ "Medium MA",
    TOT >= 50 & TSG > TMA ~ "High SG",
    TOT >= 50 & TMA > TSG ~ "High MA",
    TOT == 0 ~ "Bare",
    TRUE ~ "Unclassified"  # in case TSG == TMA or missing data
  )) %>% dplyr::filter(class != 'Unclassified')

TSet_long <- datsgma  %>%
  pivot_longer(cols = blue:DII_green_red, names_to = "band", values_to = "reflectance")
head(TSet_long)
#TSet_long <- TSet_long[-1]
colnames(TSet_long)

band_order <- c("blue", "green", "red", "vnir", 'NDAVI', 'ind24', 'NDWI', 'WAVI', 'DII_blue_green', 'DII_blue_red', 'DII_green_red') 
TSet_long$band <- factor(TSet_long$band, levels = band_order) #bands are ordered along the EM spectrum

class_stats <- TSet_long %>%
  group_by(class, band) %>%
  summarise(
    MeanSpectralValue = mean(reflectance, na.rm = TRUE),
    SD = sd(reflectance, na.rm = TRUE),
    .groups = "drop"
  )

# Plot with standard deviation as shaded area
ggplot(class_stats, aes(x = band, y = MeanSpectralValue, color = class, group = class)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MeanSpectralValue - SD, ymax = MeanSpectralValue + SD), width = 0.2) +
  labs(title = "Total cover with 15%, 30%, 45%, 60% thresholds",
       x = "Band",
       y = "Mean Surface Reflectance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"))

##Looks ok. investigate by each cover class
cllow <- class_stats %>% dplyr::filter(class %in% c('Low MA', 'Low SG'))

ggplot(cllow, aes(x = band, y = MeanSpectralValue, color = class, group = class)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MeanSpectralValue - SD, ymax = MeanSpectralValue + SD), width = 0.2) +
  labs(title = "Total cover with 15%, 30%, 45%, 60% thresholds",
       x = "Band",
       y = "Mean Surface Reflectance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"))

clmed <- class_stats %>% dplyr::filter(class %in% c('Medium MA', 'Medium SG'))

ggplot(clmed, aes(x = band, y = MeanSpectralValue, color = class, group = class)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MeanSpectralValue - SD, ymax = MeanSpectralValue + SD), width = 0.2) +
  labs(title = "Total cover with 15%, 30%, 45%, 60% thresholds",
       x = "Band",
       y = "Mean Surface Reflectance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"))

clhigh <- class_stats %>% dplyr::filter(class %in% c('High MA', 'High SG'))

ggplot(clhigh, aes(x = band, y = MeanSpectralValue, color = class, group = class)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MeanSpectralValue - SD, ymax = MeanSpectralValue + SD), width = 0.2) +
  labs(title = "Total cover with 15%, 30%, 45%, 60% thresholds",
       x = "Band",
       y = "Mean Surface Reflectance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"))

#Best classification: Bare, Low, Medium MA, Medium SG, High
datbsgma <- datall %>%
  mutate(class = case_when(
    TOT < 20 & TOT != 0 ~ 'Low',
    TOT >= 20 & TOT < 50 & TSG > TMA ~ "Medium SG",
    TOT >= 20 & TOT < 50 & TMA > TSG ~ "Medium MA",
    TOT >= 50 & TSG > TMA ~ 'High',
    TOT == 0 ~ "Bare",
    TRUE ~ "Unclassified"  # in case TSG == TMA or missing data
  )) %>% dplyr::filter(class != 'Unclassified')


TSet_long <- datbsgma  %>%
  pivot_longer(cols = blue:DII_green_red, names_to = "band", values_to = "reflectance")
head(TSet_long)
#TSet_long <- TSet_long[-1]
colnames(TSet_long)

band_order <- c("blue", "green", "red", "vnir", 'NDAVI', 'ind24', 'NDWI', 'WAVI', 'DII_blue_green', 'DII_blue_red', 'DII_green_red') 
TSet_long$band <- factor(TSet_long$band, levels = band_order) #bands are ordered along the EM spectrum

class_stats <- TSet_long %>%
  group_by(class, band) %>%
  summarise(
    MeanSpectralValue = mean(reflectance, na.rm = TRUE),
    SD = sd(reflectance, na.rm = TRUE),
    .groups = "drop"
  )

# Plot with standard deviation as shaded area
ggplot(class_stats, aes(x = band, y = MeanSpectralValue, color = class, group = class)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MeanSpectralValue - SD, ymax = MeanSpectralValue + SD), width = 0.2) +
  labs(title = "Total cover: Bare, Low (< 20%), Medium MA (20-50%, MA dom), Medium SG (20-50%, SG dom), High (> 50%)",
       x = "Band",
       y = "Mean Surface Reflectance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"))
#looks really good, but Marianna has a good one as well!
datmasg <- datall %>%
  mutate(
    TSG_Class = case_when(
      TSG > 50 ~ "High TSG",
      TSG > 20 & TSG <= 50 ~ "Medium TSG",
      TSG <= 20 ~ "Low TSG"
    ),
    TMA_Class = case_when(
      TMA > 50 ~ "High TMA",
      TMA > 20 & TMA <= 50 ~ "Medium TMA",
      TMA <= 20 ~ "Low TMA"
    ),
    Combined_Class = paste(TSG_Class, TMA_Class, sep = " / ")
  ) %>% rename(class = Combined_Class)

TSet_long <- datmasg  %>%
  pivot_longer(cols = blue:DII_green_red, names_to = "band", values_to = "reflectance")
head(TSet_long)
#TSet_long <- TSet_long[-1]
colnames(TSet_long)

band_order <- c("blue", "green", "red", "vnir", 'NDAVI', 'ind24', 'NDWI', 'WAVI', 'DII_blue_green', 'DII_blue_red', 'DII_green_red') 
TSet_long$band <- factor(TSet_long$band, levels = band_order) #bands are ordered along the EM spectrum

class_stats <- TSet_long %>%
  group_by(class, band) %>%
  summarise(
    MeanSpectralValue = mean(reflectance, na.rm = TRUE),
    SD = sd(reflectance, na.rm = TRUE),
    .groups = "drop"
  )

# Plot with standard deviation as shaded area
ggplot(class_stats, aes(x = band, y = MeanSpectralValue, color = class, group = class)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MeanSpectralValue - SD, ymax = MeanSpectralValue + SD), width = 0.2) +
  labs(title = "Total cover: Bare, Low (< 20%), Medium MA (20-50%, MA dom), Medium SG (20-50%, SG dom), High (> 50%)",
       x = "Band",
       y = "Mean Surface Reflectance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"))

#Let's k-means these groups using the separated bands - also, do pca between groups
names(datall)
dkm <- datall %>% dplyr::select(NDAVI, ind24, WAVI, DII_blue_green, DII_blue_red, DII_green_red)

dkms <- scale(dkm)


#set number of clusters
k <- 5

#run k-means
set.seed(630)

kmeans_result <- kmeans(dkm, centers = k, nstart = 25)
print(kmeans_result)

datall$cluster <- kmeans_result$cluster

plot(dkm, col = kmeans_result$cluster, pch = 20, cex = 2)
points(kmeans_result$centers, col = 1:k, pch = 4, cex = 4, lwd = 4)

##PCA visualization
pca <- prcomp(dkm, center = TRUE, scale. = TRUE)

# Take first two principal components
pca_data <- data.frame(pca$x[, 1:2])
pca_data$cluster <- as.factor(kmeans_result$cluster)

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means clustering visualized in PCA space") +
  theme_classic()

vars2 <- c('TOT', 'TSG', 'TMA', 'Tt', 'Sf', 'Hw')
  
cluster_means <- datall %>%
  group_by(cluster) %>%
  summarise(across(all_of(vars2), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

#plot histograms
for (var in vars2) {
  p <- ggplot(datall, aes_string(x = var)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "black") +
    facet_wrap(~ cluster, scales = "free_y") +
    labs(title = paste("Density of", var, "by Cluster")) +
    theme_minimal()
  
  print(p)
}

#nope, not natural. But plot them together to see separation on PCA
#Marianna's
pca <- prcomp(dkms, center = TRUE, scale. = TRUE)

# Take first two principal components
pca_data <- data.frame(pca$x[, 1:2])
pca_data$cluster <- as.factor(datmasg$class)
loadings <- pca$rotation


ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means clustering visualized in PCA space") +
  theme_classic()

#Jon's
dkm_j <- datbsgma %>% dplyr::select(NDAVI, ind24, WAVI, DII_blue_green, DII_blue_red, DII_green_red)

dkms_j <- scale(dkm_j)

pca <- prcomp(dkms_j, center = TRUE, scale. = TRUE)

# Take first two principal components
pca_data <- data.frame(pca$x[, 1:2])
pca_data$cluster <- as.factor(datbsgma$class)

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means clustering visualized in PCA space") +
  theme_classic()

#cover classes
pca <- prcomp(dkms, center = TRUE, scale. = TRUE)

# Take first two principal components
pca_data <- data.frame(pca$x[, 1:2])
pca_data$cluster <- as.factor(tot2050$class)

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means clustering visualized in PCA space") +
  theme_classic()

#scale values in original data

vars <- c("blue", "green", "red", "vnir", 'NDAVI', 'ind24', 'NDWI', 'WAVI', 'DII_blue_green', 'DII_blue_red', 'DII_green_red')

datmasg_scale <- datmasg %>%
  mutate(across(all_of(vars), ~ as.numeric(scale(.))))

TSet_long <- datmasg_scale  %>%
  pivot_longer(cols = blue:DII_green_red, names_to = "band", values_to = "reflectance")
head(TSet_long)
#TSet_long <- TSet_long[-1]
colnames(TSet_long)

band_order <- c("blue", "green", "red", "vnir", 'NDAVI', 'ind24', 'NDWI', 'WAVI', 'DII_blue_green', 'DII_blue_red', 'DII_green_red') 
TSet_long$band <- factor(TSet_long$band, levels = band_order) #bands are ordered along the EM spectrum

class_stats <- TSet_long %>%
  group_by(class, band) %>%
  summarise(
    MeanSpectralValue = mean(reflectance, na.rm = TRUE),
    SD = sd(reflectance, na.rm = TRUE),
    .groups = "drop"
  )

# Plot with standard deviation as shaded area
ggplot(class_stats, aes(x = band, y = MeanSpectralValue, color = class, group = class)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MeanSpectralValue - SD, ymax = MeanSpectralValue + SD), width = 0.2) +
  labs(title = "Total cover: Bare, Low (< 20%), Medium MA (20-50%, MA dom), Medium SG (20-50%, SG dom), High (> 50%)",
       x = "Band",
       y = "Mean Surface Reflectance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"))


dkm_j <- datbsgma %>% dplyr::select(NDAVI, ind24, WAVI, DII_blue_green, DII_blue_red, DII_green_red)

pca <- prcomp(dkm_j, center = TRUE)

# Take first two principal components
pca_data <- data.frame(pca$x[, 1:2])
pca_data$cluster <- as.factor(datbsgma$class)

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means clustering visualized in PCA space") +
  theme_classic()

datmasg <- datall %>%
  mutate(
    TSG_Class = case_when(
      TSG > 40 ~ "High TSG",
      TSG > 20 & TSG <= 40 ~ "Medium TSG",
      TSG <= 20 ~ "Low TSG"
    ),
    TMA_Class = case_when(
      TMA > 40 ~ "High TMA",
      TMA > 20 & TMA <= 40 ~ "Medium TMA",
      TMA <= 20 ~ "Low TMA"
    ),
    Combined_Class = paste(TSG_Class, TMA_Class, sep = " / ")
  ) %>% mutate(class = case_when(
    TMA_Class == 'High TMA' ~ 'High TMA',
    TSG_Class == 'High TSG' & TMA_Class != 'High TMA' ~ 'High TSG',
    TSG_Class == 'Medium TSG' & TMA_Class == 'Medium TMA' ~ 'Mixed',
    TSG_Class != 'High TSG' & TMA_Class == 'Low TMA' ~ 'Low cover',
    TSG_Class == 'Low TSG' & TMA_Class == 'Medium TMA' ~ 'Low cover', 
    TOT <= 5 ~ 'Bare'
  ))
head(datmasg)
TSet_long <- datmasg  %>%
  pivot_longer(cols = blue:DII_green_red, names_to = "band", values_to = "reflectance")
head(TSet_long)
#TSet_long <- TSet_long[-1]
colnames(TSet_long)

band_order <- c("blue", "green", "red", "vnir", 'NDAVI', 'ind24', 'NDWI', 'WAVI', 'DII_blue_green', 'DII_blue_red', 'DII_green_red') 
TSet_long$band <- factor(TSet_long$band, levels = band_order) #bands are ordered along the EM spectrum

class_stats <- TSet_long %>%
  group_by(class, band) %>%
  summarise(
    MeanSpectralValue = mean(reflectance, na.rm = TRUE),
    SD = sd(reflectance, na.rm = TRUE),
    .groups = "drop"
  )

# Plot with standard deviation as shaded area
ggplot(class_stats, aes(x = band, y = MeanSpectralValue, color = class, group = class)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MeanSpectralValue - SD, ymax = MeanSpectralValue + SD), width = 0.2) +
  labs(title = "Total cover: Bare, Low (< 20%), Medium MA (20-50%, MA dom), Medium SG (20-50%, SG dom), High (> 50%)",
       x = "Band",
       y = "Mean Surface Reflectance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"))

datbsgma <- datall %>%
  mutate(class = case_when(
    TOT < 5 ~ 'Bare',
    TOT >= 5 & TOT < 50 & TSG > TMA ~ "SG Low",
    TOT >= 5 & TOT < 50 & TMA > TSG ~ "MA Low",
    TOT >= 50 & TSG > TMA ~ 'SG High',
    TOT >= 50 & TMA > TSG ~ 'MA High',
    TRUE ~ "Unclassified"  # in case TSG == TMA or missing data
  )) %>% dplyr::filter(class != 'Unclassified')

TSet_long <- datbsgma  %>%
  pivot_longer(cols = blue:DII_green_red, names_to = "band", values_to = "reflectance")
head(TSet_long)
#TSet_long <- TSet_long[-1]
colnames(TSet_long)

band_order <- c("blue", "green", "red", "vnir", 'NDAVI', 'ind24', 'NDWI', 'WAVI', 'DII_blue_green', 'DII_blue_red', 'DII_green_red') 
TSet_long$band <- factor(TSet_long$band, levels = band_order) #bands are ordered along the EM spectrum

class_stats <- TSet_long %>%
  group_by(class, band) %>%
  summarise(
    MeanSpectralValue = mean(reflectance, na.rm = TRUE),
    SD = sd(reflectance, na.rm = TRUE),
    .groups = "drop"
  )

# Plot with standard deviation as shaded area
ggplot(class_stats, aes(x = band, y = MeanSpectralValue, color = class, group = class)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = MeanSpectralValue - SD, ymax = MeanSpectralValue + SD), width = 0.2) +
  labs(title = "Total cover: Bare, Low (< 20%), Medium MA (20-50%, MA dom), Medium SG (20-50%, SG dom), High (> 50%)",
       x = "Band",
       y = "Mean Surface Reflectance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, face = "bold"))
#This one looks good