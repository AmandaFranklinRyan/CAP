library(tidyverse)

rpg_data <- read_csv("Final Datasets//Arable Data 2007-2022 Corrected NAs.csv")
sirene_data <- read_csv("Final Datasets//Siren Data summarised.csv")
raw_conversion_list <- read_csv2("Raw Data//RPG Data//REF_CULTURES_GROUPES_CULTURES_2021.csv")
enriched_conversion_list <- read_csv("Conversion List RPG Enriched.csv")

##########################################################################
# Identify different crop categorisations and harmonise category names 
#in Sirene and RPG Data
#########################################################################

# 1. Harmonise RPG Data ---------------------------------------------------

#Identify all categories in RPG data
all_rpg <- rpg_data%>% 
  select(Description) %>% 
  distinct()
# 382 categories (2007-2015 and 2015+)

#Separate into pre and post 2015 data
categories_2007 <- all_rpg %>% 
  filter(str_detect(Description, "^[A-Z[:punct:][:space:]]+$")) #Select upper case rows (Pre 2015 data)

# Create list of categories in 2007 data
list_categories <- categories_2007 %>% 
  pull()

#Select 2015+ data categories
categories_2015 <- all_rpg %>% 
  filter(!(Description %in% list_categories)) %>% 
  drop_na()

#Select key columns from conversion list
conversion_key <- conversion_category %>% 
  select(LIBELLE_CULTURE,LIBELLE_GROUPE_CULTURE)

#Join 2015 categories with conversion list
rpg_conversion <- left_join(categories_2015,conversion_key,by=c("Description"="LIBELLE_CULTURE"))

#Export to Excel and manually enrich NA values
#write_csv(rpg_conversion,"Conversion List Sheet 2.csv")

enriched_conversion <- enriched_conversion_list %>% 
  select(DescriptionConversion= `Conversion Category`,Description2015="Description 2015")

#Join to the RPG data

harmonised_rpg <- left_join(rpg_data,enriched_conversion,by=c("Description"="Description2015"))

#Standardise category names between conversion document and 2007 data
cleaned_rpg <- harmonised_rpg %>% 
  mutate(StandardisedDescription=if_else((Year<2015),Description,DescriptionConversion)) %>%  #Replace NA entries before 2015 with original categories
  mutate(StandardisedDescription = str_to_lower(StandardisedDescription)) %>%                       # Convert all text to lowercase to match 2007 categories
  mutate(StandardisedDescription = str_replace(StandardisedDescription, "^(.)", toupper)) %>%           # capitalise first letter to match 2007 categories
  mutate(StandardisedDescription=case_when(StandardisedDescription=="Autres cereales"~"Autres céréales", #Address slight differences in category names between 2007 data and conversion document
                                           StandardisedDescription=="Autres oleagineux"~"Autres oléagineux",
                                           StandardisedDescription=="Fruits a coque"~"Fruits à coque",
                                           StandardisedDescription=="Gel (surfaces gelees sans production)"~"Gel (surfaces gelées sans production)",
                                           StandardisedDescription=="Legumes - fleurs"~"Légumes ou fleurs",
                                           StandardisedDescription=="Legumineuses a grains"~"Légumineuses à grains",
                                           StandardisedDescription=="Maïs grain et ensilage"~"Mais grain et ensilage",
                                           StandardisedDescription=="Plantes a fibres"~"Plantes à fibres",
                                           StandardisedDescription=="Proteagineux" ~"Protéagineux",
                                            TRUE~ StandardisedDescription)) %>% 
  mutate(Canton = str_replace(Canton, "^(.)", toupper)) %>% 
  select(-DescriptionConversion) %>% 
  rename(OriginalDescription=Description)

write_csv(cleaned_rpg,"Standardised RPG Data.csv")

##################################################################################
# 2. Reshape 2007-2022 into wide format  ---------------------------------------------------
##################################################################################
rpg_standardised <- read_csv("Standardised RPG Data.csv")

#Group data by broadest categories
df_2007_2021 <- rpg_standardised %>% 
  select(-OriginalDescription) %>% 
  group_by(Year,Canton,CantonCode,StandardisedDescription) %>% 
  summarise(Area=sum(Area)) %>% 
  ungroup()

df_2007_2021_wide <- df_2007_2021 %>% 
  pivot_wider(names_from = StandardisedDescription, values_from = Area)
  
write_csv(df_2007_2021_wide,"Wide data RPG 2007-2022.csv")

##################################################################################
# S. Reshape 2015-2022 into wide format  -----------------------------------------
##################################################################################

#Select all data after 2015
df_2015_plus <- rpg_standardised %>% 
  select(-StandardisedDescription) %>% 
  filter(Year>=2015)

df_2015_plus_wide <- df_2015_plus %>% 
  pivot_wider(names_from = OriginalDescription, values_from = Area)

write_csv(df_2015_plus_wide,"Wide data RPG 2015-2022.csv")


#Identify all categories in Sirene data
all_sirene <- sirene_data %>% 
  select(description) %>% 
  distinct()
#57 categories (arable and livestock)

#Define vector of livestock related activities
breeding_vector <- c("Élevage d'autres animaux","Élevage d'autres bovins et de buffles",
                     "Elevage de bovins","Élevage de vaches laitières","Élevage de porcins",
                     "Élevage de chevaux et d'autres équidés","Élevage d'ovins et de caprins",
                     "Élevage de volailles","Activités de soutien à la production animale",
                     "Elevage d'ovins, caprins et équidés","Elevage de porcins","Elevage d'autres animaux",
                     "Aviculture","Elevage de bovins associé à d'autres activités agricoles",
                     "Elevage d'ovins, caprins, équins","Services effectués au profit de l'élevage",
                     "Élevage de chameaux et d'autres camélidés","Services annexes à l'élevage",
                     "Chasse","Chasse et piégeage","Chasse, piégeage et services annexes","Elevage de volailles")

#Select arable data in Sirene
arable_sirene <- all_sirene %>% 
  filter(!(description %in% breeding_vector))

write_csv(arable_sirene,"Sirene Categories.csv")

#Identify all categories in RPG data
all_rpg <- rpg_data%>% 
  select(Description) %>% 
  distinct()
# 382 categories (2007-2015 and 2015+)

#Separate into pre and post 2015 data
categories_2007 <- all_rpg %>% 
  filter(str_detect(Description, "^[A-Z[:punct:][:space:]]+$")) #Select upper case rows (Pre 2015 data)

# Create list of categories in 2007 data
list_categories <- categories_2007 %>% 
  pull()

#Select 2015+ data categories
categories_2015 <- all_rpg %>% 
  filter(!(Description %in% list_categories)) %>% 
  drop_na()

#Join 2015 categories with conversion list
rpg_conversion <- left_join(categories_2015,conversion_key,by=c("Description"="LIBELLE_CULTURE"))

write_csv(rpg_conversion,"Conversion List Sheet 2.csv")

colnames(conversion_key)