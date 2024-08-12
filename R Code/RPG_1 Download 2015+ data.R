library(tidyverse)
library(sf)
library(gpkg)

boundaries <- st_read(dsn="Raw Data//Boundaries 2023","georef-france-canton-millesime")
boundaries_corrected <- st_read("Cleaned Data//Boundaries 2023 Corrected.shp")
crop_types <- read_csv2("Raw Data//RPG Data//REF_CULTURES_GROUPES_CULTURES_2021.csv")
crop_types_d1 <- read_csv2("Raw Data//RPG Data//REF_CULTURES_DEROBEES_2020.csv")

# 1. Check data has same format as 2022  ------------------------------------------------

st_layers("Arable 2020.gpkg")

# 9855653 rows in 2021 database
# 9778397 rows in 2020 database
# 9604463 rows in 2019 database
# 9517878 rows in 2018 database
# 9393747 rows in 2017 database
# 9334043 rows in 2016 database
# 9434672 rows in 2015 database
gpkg_contents("Raw Data//2017 RPG//Data 2018 QGIS.gpkg")
#Coordinate reference system is 2154 for 2021 data
#Coordinate reference system is RGF93 v1 / Lambert-93 (2154) for 2019 data
#Coordinate reference system is: RGF93 Lambert 93 for 2018 data
#Coordinate reference system is: RGF93 Lambert 93 for 2017 data
#Coordinate reference system is: RGF93  for 2017 data

#check column names are same as 2022 data
#gpkg_table("Arable 2021.gpkg", "parcelles_graphiques", collect = TRUE)

# 2021 column names:fid geom ID_PARCEL SURF_PARC CODE_CULTU CODE_GROUP CULTURE_D1 CULTURE_D2
# 2019 column names:  fid geom ID_PARCEL SURF_PARC CODE_CULTU CODE_GROUP CULTURE_D1 CULTURE_D2
# 2018 column names: fid  geom ID_PARCEL SURF_PARC CODE_CULTU CODE_GROUP CULTURE_D1 CULTURE_D2
# 2.Load data in chunks ----------------------------------------------

#Select 2023 boundaries
boundaries_2023 <-boundaries %>% 
  filter(year=="2023") %>% 
  select(Canton=can_name_lo,geometry,can_code)

#cumulative_df <- data.frame()
cumulative_list <- list()
chunk_count <- 1

for (x in seq(1,10000000,by=50000)){
  
  # Import data in chunks identified by fid
  min_val <- x
  max_val <- x+49999
  
  query_string <- paste("select * from parcelles_graphiques where primaryindex between ",min_val,
                        " and ",max_val,";",sep="") #primaryindex in 2007 data, fid in all other years
  
  current_chunk <- st_read("Arable 2020.gpkg",layer="parcelles_graphiques",
                           query = query_string) 
  
  #Identify centroids
  current_centroids <-  current_chunk%>% 
    mutate(Centroids=st_centroid(geom))
  
  #There is a problem with the function which changes the coordinate reference
  #system (crs) of the geometry column, it is no longer required so the code drops it
  # then converts the crs of the centroids column separately
  
  transform_centroids <- current_centroids %>% 
    as.data.frame() %>% 
    select(-geom) %>% 
    st_as_sf(crs=2154) %>% #for 2021?2019 data
    mutate(Centroids=st_transform(Centroids, 4326))
  
  current_joined <- st_join(transform_centroids,boundaries_2023,left=TRUE) %>% 
    as.data.frame()
  
  #cumulative_df <- rbind(cumulative_df, current_joined)
  cumulative_list[[chunk_count]] <- current_joined
  chunk_count <- chunk_count+1
}

cumulative_df <- do.call(rbind,cumulative_list)

write_csv(cumulative_df,"Arable 2020 RPG.csv")



# 2. Clean and Group data -------------------------------------------------

raw_2022 <- read_csv("Cleaned Data//Backup Data//Arable 2022 RPG.csv")

#Convert to df to reduce size as centroid data not needed
df_2022 <- raw_2022 %>% 
  as.data.frame() %>% 
  select(-CODE_GROUP,-Centroids)

#Create a variable for calculating number of crops
df_crops <- df_2015 %>% 
  mutate(count_d1=if_else(!is.na(CULTURE_D1),1,NA)) %>% 
  mutate(count_d2=if_else(!is.na(CULTURE_D2),1,NA)) %>% 
  mutate(total_count=count_d1+count_d2) %>% 
  select(-count_d1,-count_d2)

table(df_crops$total_count)
rm(df_2015)

# Create single column for crop type (currently 3 for first, second and main crop)
#In 2022,2021 and 2019,2018,2017,2016(268528 ) data only main crop or two secondary crops
longer_2015<- df_crops %>% 
  pivot_longer(cols=CODE_CULTU:CULTURE_D2,
               names_to="cultureType",
               values_to="Code")

rm(longer_2015)

#Drop rows where no secondary crops and assign land areas to plots with secondary crops
crops_2015 <- longer_2015 %>% 
  filter(!(cultureType=="CULTURE_D1"&is.na(Code))&!(cultureType=="CULTURE_D2"&is.na(Code))) %>% 
  mutate(area=case_when((cultureType=="CODE_CULTU"&total_count==2)~0.7*SURF_PARC,#2 additional crops, 70% area first crop
                        (cultureType=="CODE_CULTU"&total_count==1)~0.7*SURF_PARC, #1 additional crop, 70% area first crop
                        (cultureType=="CULTURE_D1"&total_count==1)~0.3*SURF_PARC,#1 additional crop, 30% first crop
                        (cultureType=="CULTURE_D1"&total_count==2)~0.2*SURF_PARC,#2 additional crops, 20% first crop
                        (cultureType=="CULTURE_D2"&total_count==2)~0.1*SURF_PARC,#2 additional crops, 10% second crop
                        (is.na(total_count)~SURF_PARC))) # If only one crop then area to whole crop plot


table(crops_2021$total_count)
#Group data by canton
canton_2015 <- crops_2015 %>% 
  group_by(can_code,Canton,Code,cultureType) %>% 
  summarise(totalArea=sum(area)) %>% 
  ungroup()

write_csv(canton_2015,"Grouped Arable Land 2015 new.csv")


# Found 3 pairs of cantons with the same name, join to renamed boundaries to correct this
boundaries_short <- boundaries_corrected %>% 
  select(can_cod,cn_nm_l) %>% 
  as.data.frame() %>% 
  select(-geometry)

corrected_2015 <- left_join(canton_2015,boundaries_short,by=c("can_code"="can_cod")) %>% 
  select(-Canton) %>% 
  rename(Canton=cn_nm_l)

data_2020 <- read_csv("Cleaned Data//Backup Data//Grouped Arable Land 2020 new.csv")

data_2022 <- read_csv("Cleaned Data//Backup Data//Grouped Arable Land 2022 Version 3.csv")
  
# 4. Add crop labels to agricultural data ---------------------------------

#Select key columns from crop label data
crop_labels <- crop_types %>% 
  select(CODE_CULTURE,LIBELLE_CULTURE)

#Join both sets of crop labels on arable data 
labelled_2015 <- left_join(corrected_2015,crop_labels,by=c("Code"="CODE_CULTURE"))
labelled_d1 <- left_join(labelled_2015,crop_types_d1,by=c("Code"="CODE_CULTURE_DEROBEE"))

#Create column with correct description for each crop type
arable_2015_labelled <- labelled_d1 %>% 
  mutate(description=if_else(cultureType=="CODE_CULTU",LIBELLE_CULTURE,LIBELLE_CULTURE_DEROBEE)) %>% 
  select(-cultureType,-LIBELLE_CULTURE,-LIBELLE_CULTURE_DEROBEE,-Code) %>% 
  group_by(Canton,description) %>% #Grouped by code before, but could have same descriptions in different versions of the code
  summarise(Area=sum(totalArea)) %>% 
  ungroup() %>% 
  mutate(Year=2015)

write_csv(arable_2015_labelled,"Arable Land 2015 Corrected.csv") 




