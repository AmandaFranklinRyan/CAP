library(tidyverse)
library(plotly)
library(sf)

# Select 2023 data and modify canton name so they unique (currently 3 pairs of duplicates)
boundaries <- st_read(dsn="Raw Data//Boundaries 2023","georef-france-canton-millesime")

boundaries_2023 <- boundaries %>% 
  filter(year==2023)

#Identify duplicate labels
duplicates <- boundaries_2023 %>% 
  group_by(can_name_lo) %>% 
  filter(n()>1) %>% 
  ungroup()

#& reg_code=="['La Réunion']

#Specify unique labels
corrected <- boundaries_2023 %>% 
  mutate(can_name_lo=if_else((can_name_lo=="saint-denis-2" & can_area_co=="REU"),"saint-denis-2 REU",can_name_lo)) %>%
  mutate(can_name_lo=if_else((can_name_lo=="saint-denis-1" & can_area_co=="REU"),"saint-denis-1 REU",can_name_lo)) %>% 
  mutate(can_name_lo=if_else((can_name_lo=="saint-vallier" & can_id==28),"saint-vallier BFC",can_name_lo))

#Export df for use in other files
st_write(corrected, "Cleaned Data//Boundaries 2023 Corrected.shp")

# 2. Extra Checks on 2014 data --------------------------------------------

#Data format changes between 2014 and 2015
# Discrepancies in the number of rows in the final grouped dfs
# In 2014+ data around 100,000 rows, but in 2014 only around 20000
#Partly due to different group categories, lant fewer prior to 2014

arable_2014 <- read_csv("Grouped Arable Land 2014 RPG.csv")
arable_2015 <- read_csv("Labelled Arable Land 2015.csv")
arable_2013 <- read_csv("Arable Land 2013 Corrected.csv")
arable_2012 <- read_csv("Arable Land 2012 Corrected.csv")

#Calculate difference in area

canton_2012 <- arable_2012 %>% 
  group_by(Canton) %>% 
  summarise(Area_2012=sum(Area, na.rm=TRUE))

canton_2012_total <- canton_2012 %>% 
  summarise(Total=sum(Area_2012))
# 23595667 hectares total

canton_2013 <- arable_2013 %>% 
  group_by(Canton) %>% 
  summarise(Area_2013=sum(Area, na.rm=TRUE))

canton_2013_total <- canton_2013 %>% 
  summarise(Total=sum(Area_2013))
# 23848749 hectares total

canton_2014 <- arable_2014 %>% 
  group_by(Canton) %>% 
  summarise(Area_2014=sum(Area, na.rm=TRUE))

canton_2014_total <- canton_2014 %>% 
  summarise(Total=sum(Area_2014))
# 23748506 hectares total

canton_2015 <- arable_2015 %>% 
  group_by(Canton) %>% 
  summarise(Area_2015=sum(Area, na.rm=TRUE))

# 27858410 hectares total

canton_2015_total <- canton_2015 %>% 
  summarise(Total=sum(Area_2015))

area_2014_2015 <- left_join(canton_2014,canton_2015) %>% 
  mutate(difference=Area_2015-Area_2014) %>% 
  ungroup() $

area_difference <- area_2014_2015 %>% 
  summarise(Difference=sum(difference, na.rm=TRUE))
# There is a difference in area of 4109902 hectares
# Possibly due to the fact that secondary crop data is only recorded from 2014 onwards

# 2. Clean and Group data -------------------------------------------------

#Calculate the area devoted to secondary crops in 2015 data

raw_2015 <- read_csv("Arable 2015 RPG.csv")

rm(df_crops)

#Convert to df to reduce size as centroid data not needed
df_2015 <- raw_2015 %>% 
  as.data.frame() %>% 
  select(-CODE_GROUP,-Centroids)

#Create a variable for calculating number of crops
df_crops <- df_2015 %>% 
  mutate(count_d1=if_else(!is.na(CULTURE_D1),1,NA)) %>% 
  mutate(count_d2=if_else(!is.na(CULTURE_D2),1,NA)) %>% 
  mutate(total_count=count_d1+count_d2) %>% 
  select(-count_d1,-count_d2)

# Create single column for crop type (currently 3 for first, second and main crop)
#In 2022,2021 and 2019,2018,2017,2016(268528 ) data only main crop or two secondary crops
longer_2015 <- df_crops %>% 
  pivot_longer(cols=CODE_CULTU:CULTURE_D2,
               names_to="cultureType",
               values_to="Code")

#Drop rows where no secondary crops and assign land areas to plots with secondary crops
crops_2015 <- longer_2015 %>% 
  filter(!(cultureType=="CULTURE_D1"&is.na(Code))&!(cultureType=="CULTURE_D2"&is.na(Code))) %>% 
  #mutate(area=if_else(is.na(total_count),surf_parc,surf_parc/total_count)) #half area for each secondary crop, same area for single crop
  mutate(area=case_when((cultureType=="CODE_CULTU"&total_count==2)~0.7*SURF_PARC,#2 additional crops, 70% area first crop
                        (cultureType=="CODE_CULTU"&total_count==1)~0.7*SURF_PARC, #1 additional crop, 70% area first crop
                        (cultureType=="CULTURE_D1"&total_count==1)~0.3*SURF_PARC,#1 additional crop, 30% first crop
                        (cultureType=="CULTURE_D1"&total_count==2)~0.2*SURF_PARC,#2 additional crops, 20% first crop
                        (cultureType=="CULTURE_D2"&total_count==2)~0.1*SURF_PARC,#2 additional crops, 10% second crop
                        (is.na(total_count)~SURF_PARC))) # If only one crop then area to whole crop plot

secondary_crop_area <- crops_2015 %>% 
  filter(total_count==2) %>% 
  group_by(Canton) %>% 
  summarise(Area=sum(SURF_PARC)) %>% 
  ungroup() %>% 
  arrange(desc(Area)) %>% 
  head(15)

secondary_crop_area$Canton

# Areas with most secondary crops
#[1] "les villages vovéens"   "saint-valery-en-caux"   "la guerche-de-bretagne" "broons"                
#[5] "arcis-sur-aube"         "marle"                  "avesnes-le-comte"       "pithiviers"            
#[9] "le cateau-cambrésis"    "le saulnois"            "châtillon-sur-seine"    "cossé-le-vivien"       
#[13] "neufchâtel-en-bray"     "la châtaigneraie"       "val-couesnon"

write_csv(secondary_crop_area,"Secondary Crops 2015.csv")

secondary_crop_total <- crops_2015 %>% 
  filter(total_count==2) %>% 
  summarise(Total=sum(SURF_PARC, na.rm=TRUE))
# 3650088 hecatres of secondary crops

#>Les villages voveens has the highest increase in crop land between 2014 and 2015 and the
#largest area of secondary crops