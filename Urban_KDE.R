library(sf)
library(ggplot2)
library(dplyr)
library(raster)
library(MASS)
library(rgdal)
library(dplyr)
library(data.table)
library(randomForest)
library(leaps)

# Read a POI layer
poi <- st_read("C:/Users/siliezar-montoya/Documents/Documentos/TheseUMRAE/These/VoiceModel/Layers/POIS_TLS.shp")
# Display the TYPE column contained in the POI layer
types <-  unique(poi$TYPE)
types

####################
## CONFIG KDE
####################

# Grouping the POI dataset by TYPE and summarizing the count of each type of POI
poi %>% group_by(TYPE) %>%  summarize(count=n()) %>% dplyr::select(TYPE, count) %>%  arrange(desc(count))
# Calculate the bounding box (extent) of the POI dataset
extent_zone <- st_bbox(poi)
# Determine the number of cells in the x and y directions based on the bounding box and a specified cell size
reordered_corners_coordinates <-  c(extent_zone$xmin,extent_zone$xmax, extent_zone$ymin,extent_zone$ymax )
cellsize <-  150 # en mètres
n_x <- ceiling((extent_zone$xmax  -extent_zone$xmin)/cellsize)
n_y <- ceiling((extent_zone$ymax -extent_zone$ymin)/cellsize)
# Print the dimensions of the raster grid
cat("raster de", n_x,"x",n_y, " cellules"  )

####################
# COMPTAGES
####################

# Read the layer containing the counting of pedestrians in Toulouse
ped_vol <-  st_read("C:/Users/siliezar-montoya/Documents/Documentos/TheseUMRAE/These/VoiceModel/DONNEES TLS/comptages-pietons.shp")
# Converts the 'comptage' column to numeric
ped_vol$comptage <- as.numeric(ped_vol$comptage)
# Transforme CRS to EPSG 2154
ped_vol <- st_transform(ped_vol, 2154)
# Intersect 'ped_vol' with the bounding box in order to obtain only the points inside of the extent box
ped_vol <-  st_intersection(ped_vol,st_as_sfc(extent_zone))
ped_vol <-  st_as_sf(ped_vol)
#plot(ped_vol$geometry, cex=0.2)

# Filtering 'ped_vol' based on year, time of the day and day of the week. This can be modified in function of the study
ped_vol <-  ped_vol %>%  dplyr::filter(moment_dans=="journee", jours=="semaine")
ped_vol
#plot(densityKDE)
#plot(food_drink_poi$geometry,add=T, cex=0.2)
#plot(ped_vol$geometry, add=T, cex=0.6, col=2)

# Extraction X and Y from 'ped_vol' using st_coordinates
ped_vol_coords <-  st_coordinates(ped_vol)
ped_vol_X <- ped_vol_coords[,1]
ped_vol_Y <- ped_vol_coords[,2]
ped_vol_xy <- cbind(ped_vol_X, ped_vol_Y)
types

####################
##  KDE
####################

# Looping through each POI type in the 'types' vector
for (t in types){
  # Filtering the POI dataset for the current type and extracting its coordinates
  # food_drinks
  food_drink_poi<-c()
  food_drink_poi <-  poi %>%filter(TYPE==t)
  # Extract coordinates
  coords_poi <- st_coordinates(food_drink_poi)
  # Calculat a 2D kernel density estimation. Creating a density matrix
  matrix_kde_poi <- kde2d(coords_poi[,"X"],
                          coords_poi[,"Y"],
                          h = rep(250, 250),
                          n = c(n_x,n_y),
                          lims = reordered_corners_coordinates)
  # Raster object with Z attributes of the kde2D object (CRS 2154)
  densityKDE <-  raster(matrix_kde_poi,crs=CRS("+init=epsg:2154"))
  # Extraction of the KDE values for each pedestrian volume point
  ped_vol[t]<-raster::extract(densityKDE, ped_vol_coords, method = 'bilinear')
}

# Convert extracted KDE to data frame and select columns of interest
ped_vol_filt <-ped_vol %>% as.data.frame %>% dplyr::select(comptage, food_drink , footpath , public_transport, shop, tourism, tourism_sleep, education, culture, leisure, individual_transport, religion, trees, sport)
#ped_vol_filt$comptage <- log10(ped_vol_filt$comptage+0.01)
# Modify the counting column to filter out values less than or equal to zero
#ped_vol_filt$comptage <- ped_vol_filt %>% filter(comptage>0)
#ped_vol_filt2 <- ped_vol_filt %>% filter(comptage>0)
# Perform a generalized linear model GLM analysis on the filtered datahttp://127.0.0.1:29983/graphics/plot_zoom_png?width=1162&height=752
glm1<-randomForest(data=ped_vol_filt, comptage~food_drink + footpath + public_transport + shop + tourism + tourism_sleep + education + culture
                   + leisure + individual_transport + religion + trees + sport)

summary(glm1)
varImpPlot(glm1)





#### TEST ####

# Best subsets
# http://r-statistics.co/Model-Selection-in-R.html
# This is a technique that relies on stepwise regression to search, find and visualise regression models. Unlike stepwise regression, we can see what variables
# were included in various shortlisted models, force-in or force-out some of the explanatory variables and also visually inspect the model's performace

# Model preparation

response_df <- ped_vol_filt['comptage']
predictors_df <- ped_vol_filt[, !names(ped_vol_filt) %in% "comptage"]

#subsets_model <- regsubsets(x=predictors_df, y=response_df, nbest = 2, really.big = TRUE)
subsets_model <- regsubsets(comptage~food_drink + public_transport + shop + tourism + tourism_sleep + education + culture + leisure + individual_transport + religion + trees + sport, data = ped_vol_filt, nbest = 2, really.big = T)
summary(subsets_model)
plot(subsets_model, scale = "adjr2")

#-----
# Test model
# ped_vol is the dependent variable, shop and food_drink are the independent variables (predictors)
model <- lm(comptage ~ shop + food_drink, data = ped_vol_filt)

# Model training and evaluation
# In machine learning, it's common practice to split the dataset into 2 subsets: One for training the model, the other one for testing its performance.
train_idx <- sample(1:nrow(ped_vol_filt), nrow(ped_vol_filt)*0.8) #80% for training
test_idx <- setdiff(1:nrow(ped_vol_filt), train_idx)  # 20% for testing, this creates a set of indices that are not included in the training set

train_data <- ped_vol_filt[train_idx,]
test_data <- ped_vol_filt[test_idx,]

model <- lm(comptage~food_drink + footpath +public_transport + shop + tourism + tourism_sleep + education + culture
            + leisure + individual_transport + religion + trees + sport, data = train_data)
summary(model)
plot(model$model)
