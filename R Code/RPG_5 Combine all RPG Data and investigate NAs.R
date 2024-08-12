library(tidyverse)
library(plotly)

# 1. Combine data from annual arable files --------------------------------

folder_path <- paste("Cleaned Data//Annual Arable Files")
boundaries_corrected <- st_read("Cleaned Data//Boundaries Corrected//Boundaries 2023 Corrected.shp")

#List all files in data folder
list_files <- list.files(folder_path,full.names=TRUE)

cumulative_list <- list()
chunk_count <- 1

for (file in list_files){
  
  current_file <- read_csv(file) %>% 
    select(Canton,description,Area,Year)
  
  cumulative_list[[chunk_count]] <- current_file
  chunk_count <- chunk_count+1
}

cumulative_df <- do.call(rbind,cumulative_list)

#Add canton codes to RPG data
boundaries_summary <- boundaries_corrected %>% 
  select(cn_nm_l,can_cod)

all_arable_final <- left_join(all_arable,boundaries_summary,by=c("Canton"="cn_nm_l")) %>% 
  mutate(CantonCode = str_replace_all(can_cod, "[[:punct:]]", "")) %>% 
  select(Canton,CantonCode,Description=description,Area,Year)

write_csv(all_arable_final,"Final Datasets//Arable Data 2007-2022 Corrected NAs.csv")

# 2. Check data NAs -------------------------------------------

# 3. Check NAs in the canton column -------------

#Check how many entries have NAs in the Canton column
nas_df <- all_arable %>% 
  filter(is.na(Canton))

table(nas_df$Year)

#Canton NAs
nas_2020 <- data_2020 %>% 
  filter(is.na(Canton))
#Around 50 NAs in canton from 2007-2014 and around 100 in later data


#Check NAs in 2020 data
#All rows have centroids so no missing geometry column
#So why are these points not being assigned to a canton?
data_2020 <- read_csv("Cleaned Data//Backup Data//Arable 2020 RPG.csv")
(nrow(nas_2020)/nrow(data_2020))*100
#0.05 of 2020 data is NAs

#Export NA centroids and explore further
#Extract latitude and longitude into separate columns for Google Maps
na_coords <- nas_2020 %>% 
  select(SURF_PARC,Centroids) %>% 
  mutate(latitude = str_extract(Centroids, "\\d+\\.\\d+(?=\\))")) %>% 
  mutate(longitude = str_extract(Centroids, "(?<=\\()-?\\d+\\.\\d+(?=,)")) %>% # Extract latitude (first number in the string)
  select(-Centroids,-SURF_PARC)

coordinates <- write_csv(na_coords," NA Coordinates.csv")

#Uploading coordinates into Google maps shows centroids have been placed on the wrong side of boundaries
# In 2020 constituting 0.04% of total data

# 3. Check NAs in the description column -------------

#Select rows with NA in description column
nas_desc_df <- all_arable %>% 
  filter(is.na(description))

#Investigate NA distribution over time
table(nas_desc_df$Year)
# 2007 2008 2009 2010 2011 2012 2013 2014 2020 2021 2022 
# 790  714  710  293  310  332  308  238   22  186   62 

arable_2007 <- read_csv("Annual Arable Files//Arable Land 2007 Corrected.csv")
# NAs present in the original data file in 2007-2014 data

nas_desc_2020 <- data_2020 %>% 
  filter(is.na(CODE_CULTU))
# No missing data in CODE_CULTU
#Presumably NAs must be added at teh labelling stage

#Check 2020 data
data_2020 <- read_csv("Cleaned Data//Backup Data//Grouped Arable Land 2020 new.csv")
crop_types <- read_csv2("Raw Data//RPG Data//REF_CULTURES_GROUPES_CULTURES_2021.csv")
crop_types_d1 <- read_csv2("Raw Data//RPG Data//REF_CULTURES_DEROBEES_2020.csv")

#Select key columns from crop label data
crop_labels <- crop_types %>% 
  select(CODE_CULTURE,LIBELLE_CULTURE)

#Join both sets of crop labels on arable data 
labelled_2020 <- left_join(data_2020,crop_labels,by=c("Code"="CODE_CULTURE"))
labelled_d1 <- left_join(labelled_2020,crop_types_d1,by=c("Code"="CODE_CULTURE_DEROBEE"))

both_na <- labelled_d1 %>% 
  filter(is.na(LIBELLE_CULTURE) & is.na(LIBELLE_CULTURE_DEROBEE))

#In 2020 data NAs are introduced as DCI and DNR have no corresponding labels in CODE_CULTURE or
# CODE_CULTURE_DEROBEE data

table(both_na$Code)
#DCI DNR 
#118 127

(nrow(both_na)/nrow(labelled_d1))*100
#These NAs constitute 0.2% of the entire dataset

#calculate fraction of NAs in final dataset

data_no_nas <- all_arable %>% 
  drop_na()

(1-(nrow(data_no_nas)/nrow(all_arable)))*100
#0.5% of data NAs
