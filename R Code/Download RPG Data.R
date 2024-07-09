library(tidyverse)
library(sf)
library(rio)

crop_types <- read_csv2("Raw Data\\REF_CULTURES_GROUPES_CULTURES_2021.csv")
crop_data_2021 <- st_read(dsn="Raw Data\\2021","PARCELLES_GRAPHIQUES") #Area is measured in hectares
boundaries <- st_read(dsn="Raw Data\\Boundaries 2023","georef-france-canton-millesime")

#Identify projection of crop data: EPSG:2154 
st_crs(crop_data_2021)$srid

#Identify projection of boundary data: EPSG:4326
st_crs(boundaries)$srid

#Select 2023 boundaries
boundaries_2023 <-boundaries %>% 
  filter(year=="2023") %>% 
  select(Canton=can_name_lo,geometry)

#Calculate centroids
crop_centroids <-  crop_data_2021%>% 
  mutate(Centroids=st_centroid(geometry)) %>% 
  st_transform(4326) %>% #Convert projection of geometry column
  mutate(Centroids=st_transform(Centroids, 4326)) #Convert geometry of centroids column

centroids2 <- crop_centroids %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  st_as_sf()
  
#Join data
joined <- st_join(boundaries_2023,centroids2,left=TRUE)


test2 <- joined %>%
  as.data.frame() %>%
  select(-geometry)

test3 <- test2 %>% 
  filter(Canton=="saint-bonnet-en-champsaur")


number_canton <- joined %>%  
  as.data.frame() %>%
  group_by(Canton) %>% 
  summarise(Count=n())




  filter(is.na(ID_PARCEL)) %>% 
  summarise(Count=n())
  

test <- crop_centroids %>% 
  as.data.frame() %>% 
  select(ID_PARCEL, Centroids)

colnames(test)
  

centroids <- boundaries_2023 %>% 
  mutate(Centroids=st_centroid(geometry)) %>% 
  select(-geometry)

st_write(centroids, "Centroids2.shp", append=FALSE)
st_write(boundaries_2023, "Boundaries.shp")
st_write(coordinates, "coordinates.shp")
st_write(test, "centroids3.shp")


head(boundaries_2023)

table(boundaries$STATUT)