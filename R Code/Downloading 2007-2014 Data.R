library(tidyverse)
library(sf)

#Import 2023 boundaries file with no duplicates
boundaries <- st_read("Cleaned Data//Boundaries 2023 Corrected.shp")

# Set year of interest and folder path
year <- "2008"
folder_path <- paste("Raw Data//2018 RPG//1_DONNEES_LIVRAISON_",year,sep="")

#List all files in data folder
list_files <- list.files(folder_path,full.names=TRUE)

#Select only subfolders (not md5 files)
file_info <- file.info(list_files)
subfolders_only <- list_files[file_info$isdir]

#Identify files in LAMB93 CRS as this means they are in France not overseas territories
france_string <- "LAMB93"
french_files <- subfolders_only[grep(france_string,subfolders_only)]

#Create list to store dfs in in the loop and counting variable to move through list elements
cumulative_list <- list()
chunk_count <- 1

for (file in french_files){
  
  current_file_name <- paste(file,"//ILOTS_ANONYMES.shp",sep="")
  #Extract region number from file name and add to df as extra check that all regions are present
  region_pattern <- "LAMB93_R0"
  region_number <- str_extract(current_file_name,paste0(region_pattern,"\\d{1,2}"))
  
  current_file <- st_read(current_file_name)
  #Extract key columns
  # Have commune data, but as commune names changes continue to use centroid geometry to ascertain canton
  #Assumed SURF_CULTU is area of cultivated plot while SURF_GRAPH is area of the plot (generally same or slightly
  # smaller than SURF_GRAPH)
  
  #Select key columns and convert "NR" to NA and correct types
  key_data <- current_file %>% 
    select(NUM_ILOT,NOM_CULTU) %>% #SURF_CULTU
    #mutate(COMMUNE=if_else(COMMUNE=="NR",NA,COMMUNE)) %>% # Not present in 2008 and 2009 data
    mutate(NOM_CULTU=if_else(NOM_CULTU=="NR",NA,NOM_CULTU)) %>% 
    #mutate(SURF_CULTU=if_else(SURF_CULTU=="NR",NA,SURF_CULTU)) %>% # Not present in 2008 and 2009 data
    #mutate(SURF_CULTU=as.numeric(SURF_CULTU)) %>%  # Not present in 2008 and 2009 data
    mutate(area_sqm=st_area(geometry)) %>% #calculate area in hectares for 2008 and 2009 data
    mutate(SURF_CULTU=round(as.numeric(area_sqm)/10000,2))
  
  #Select 2023 cantonal boundaries
  boundaries_2023 <-boundaries %>% 
    select(Canton=cn_nm_l,geometry,can_cod)
  
  #Check CRS of dataframe 
  #st_crs(key_data)
  #RGF93 v1 / Lambert-93
  
  #Calculate centroids and change crs to match cantonal boundaries
  centroids <- key_data %>% 
    mutate(Centroids=st_centroid(geometry)) %>% 
    as.data.frame() %>% 
    select(-geometry) %>% #problem with CRS conversion so convert to df and back to spatial df
    st_as_sf(crs=2154) %>% #current crs
    mutate(Centroids=st_transform(Centroids, 4326)) #transform to this crs
  
  #Jon centroids with 2023 canton boundaries
  canton_centroids <- st_join(centroids,boundaries_2023,left=TRUE) %>% 
    as.data.frame()
  
  #Calculate cultivated area for each region by canton
  grouped_crops <- canton_centroids %>% 
    group_by(can_cod,Canton,NOM_CULTU) %>% 
    summarise(totalArea=sum(SURF_CULTU)) %>% 
    ungroup() %>% 
    select(Canton,Code=can_cod,description=NOM_CULTU,Area=totalArea) %>% 
    mutate(Year=year) 
  
  cumulative_list[[chunk_count]] <- grouped_crops
  chunk_count <- chunk_count+1
}

#Bind list of dataframes from individual files together
cumulative_df <- do.call(rbind,cumulative_list)

write_csv(cumulative_df,"Arable Land 2008 Corrected.csv")

