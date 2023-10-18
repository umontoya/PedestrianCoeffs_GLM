library(jsonlite)
library(geojsonR)
library(rgdal)
library(dplyr)
library(sf)
library(ggplot2)
library(MASS)
library(raster)
library(data.table)

setwd("C:/Users/siliezar-montoya/Documents/Documentos/TheseUMRAE/These/VoiceModel")

# Import the layers to use
#PEDESTRIAN_POIS: Points of interest in TLS
#COMPTAGE_TLS: Pedestrian count in TLS
#PEDESTRIAN_WAYS: Pedestrian ways (Network)
#PEDESTRIAN_AREAS: Pedestrian ways (Buffer - Zone Marchable)
PEDESTRIAN_POIS <- st_read("C:/Users/siliezar-montoya/Documents/Documentos/TheseUMRAE/These/VoiceModel/DONNEES TLS/POIS_TLS.shp")
PEDESTRIAN_WAYS <- st_read("C:/Users/siliezar-montoya/Documents/Documentos/TheseUMRAE/These/VoiceModel/DONNEES TLS/PEDESTRIAN_WAYS.shp")
COMPTAGE_TLS <- st_read("C:/Users/siliezar-montoya/Documents/Documentos/TheseUMRAE/These/VoiceModel/DONNEES TLS/comptages-pietons.shp")

#Points of interest
BOUTIQUES <- subset(PEDESTRIAN_POIS, TYPE == "shop")
RESTAURANTS <- subset(PEDESTRIAN_POIS, TYPE == "food_drink")
TRANSPORT <- subset(PEDESTRIAN_POIS, TYPE == "public_transport")
RELIGION <- subset(PEDESTRIAN_POIS, TYPE == "religion")
CULTURE <- subset(PEDESTRIAN_POIS, TYPE == "culture")

#Plotting the POIs in the study area
plotzone <-  ggplot()+
  geom_sf(data=PEDESTRIAN_WAYS, color= "gray50", fill=NA, lwd=0.1)+
  geom_sf(data=RESTAURANTS, aes(color="restaurants"), size= 0.1)+
  geom_sf(data=BOUTIQUES, aes(color="boutiques"), size=0.1)+
  geom_sf(data=TRANSPORT, aes(color="tramway"), size= 1)+
  geom_sf(data=RELIGION, aes(color="religion"), size= 1)+
  geom_sf(data=CULTURE, aes(color="culture"), size= 1)+
  theme_light()+
  coord_sf(default_crs = sf::st_crs(2154))+
  scale_color_manual(name= "Points d'intérêt",  
                     breaks= c("restaurants", "boutiques", "tramway","religion","culture"),
                     values = c("restaurants" = "orange", 
                                "boutiques" = "chartreuse3",
                                "tramway" = "darkorchid",
                                "religion" = "red",
                                "culture" = 'blue')) 

plotzone

#Bounding box for the study area
extent_zone <- st_bbox(PEDESTRIAN_WAYS)
extent_zone

#kde2d function receives the coin coordinates of the raster (lims argument) on a different order than the one given by fonction st_bbox
reordered_corners_coordinates <-  c(extent_zone$xmin,extent_zone$xmax, extent_zone$ymin,extent_zone$ymax )
reordered_corners_coordinates

#Calculate the number of cells of the raster in the study are from a given size of a cell
cellsize <-  10 # en mètres
n_x <- ceiling((extent_zone$xmax  -extent_zone$xmin)/cellsize)
n_y <- ceiling((extent_zone$ymax -extent_zone$ymin)/cellsize)
cat("raster de", n_x,"x",n_y, " cellules"  )

#We recover the coordinates X and Y of the POIs dataset (Restaurants)
coords_resto <- st_coordinates(RESTAURANTS)
matrix_resto <- kde2d(coords_resto[,"X"],
                      coords_resto[,"Y"], 
                      n = c(n_x,n_y),
                      lims = reordered_corners_coordinates)
densite_resto <-  raster(matrix_resto)
plot(densite_resto)

#We define a function to use any type of POIs
#Bandwidth function is determinde automatically by an R function
#But it could be useful to define it ourselves to refine/smooth the density
densite_raster <-  function(semis, nX, nY, corners_coordinates, bandwidth=NULL){
  x <- st_coordinates(semis)[,"X"]
  y <- st_coordinates(semis)[,"Y"]
  matrix_dens <- kde2d(x,y,
                       h= ifelse(is.null(bandwidth),c(bandwidth.nrd(x), bandwidth.nrd(y)) , bandwith),  
                       n = c(nX,nY),
                       lims = corners_coordinates)
  resultat <- raster(matrix_dens)
  projection(resultat) <-  projection(semis)
  return(resultat)
}

library(sfhotspot)
hexa_dens_resto <-  hotspot_kde(RESTAURANTS, cell_size = 10, grid_type = "hex")
hexa_dens_bouti <-  hotspot_kde(BOUTIQUES, cell_size = 10, grid_type = "hex")
hexa_dens_tram <-  hotspot_kde(TRANSPORT, cell_size = 10, grid_type = "hex")


par(mfrow=c(1,3))
plot(hexa_dens_resto["kde"])
plot(hexa_dens_bouti["kde"])
plot(hexa_dens_tram["kde"])

# Filter out some of the classes of the dataframe. We want to keep only the pedestrian ways (Refer to the description PDF file)
unique(streets$fclass)
filt_streets <- streets[streets$fclass %in% c('unclassified', 'residential', 'living_street', 'pedestrian'),] #We obtain a layer with only pedestrian roads

# Write it as shape file
st_write(filt_streets, "C:/Users/siliezar-montoya/Documents/Documentos/TheseUMRAE/These/VoiceModel/Script Paul_Pierre/UrbanNetwork2.shp")


# Create a spatial index
index <- st_create_spatial_index(filt_streets)

# Use the index for intersections
intersections <- st_intersection(urban_network, urban_network, sparse = TRUE, indices = index)


# Identify road intersections
intersections <- st_intersection(filt_streets, filt_streets)

# Create an empty data frame to store the segmented network
segmented_network <- data.frame()

# Loop through each intersection
for (i in 1:length(intersections)) {
  # Get the coordinates of the intersection
  intersection_coords <- st_coordinates(intersections[i, ])
  
  # Extract the segments connected to the intersection
  connected_segments <- filt_streets[st_touches(filt_streets, intersections[i, ]), ]
  
  # Split the connected segments at the intersection
  split_segments <- st_split(connected_segments, intersections[i, ])
  
  # Add the split segments to the segmented network
  segmented_network <- rbind(segmented_network, split_segments)
}



##
# Create an empty dataframe to store the segmented network
urban_net <- st_sfc()

# Iterate over each segment in the pedestrian network
for (i in 1:length(filt_streets$geometry)) {
  segment <- filt_streets[i, ]
  
  # Get the beginning and end points of the segment
  segment_coords <- st_coordinates(segment$geometry)
  segment_start <- st_point(segment_coords[1, ])
  segment_end <- st_point(segment_coords[nrow(segment_coords), ])
  
  # Create a new line segment with the start and end points as its geometry
  new_segment <- st_linestring(rbind(segment_start, segment_end))
  
  # Add additional attributes from the original segment if needed
  # new_segment <- cbind(new_segment, segment$attribute_name)
  
  # Append the new segment to the segmented network dataframe
  urban_net <- rbind(urban_net, new_segment)
}

# Convert the segmented network to an sf object
#urban_net <- st_as_sf(urban_net, coords = c("X", "Y"), crs = st_crs(filt_streets))

# View the segmented network
plot(urban_net)



