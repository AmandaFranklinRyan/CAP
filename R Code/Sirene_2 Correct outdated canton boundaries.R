library(tidyverse)
library(DBI) #fpr connecting to SQLite
library(RSQLite) #fpr connecting to SQLite

# 1. Collate all geographic Data ------------------------------------------

#Imports list of all agriculture related enterprises from code file:
enterprise_list <- read_csv("Cleaned Data//Agricultural Enterprises List.csv")
enterprise_vector <- as.integer(enterprise_list$siren)

#Import commune/canton data from: https://www.insee.fr/fr/information/6800675
cantons <- read_csv("Raw Data//Canton Boundaries//Commune Data.csv")

#Download event data from: https://www.insee.fr/fr/information/6800675
# This shows historic changes to canton numbers and names
events <- read_csv("Raw Data//Sirene Data//Event Data.csv")

# 1. Create mapping of communes to 2023 cantons ---------------------------

# Delete all canton codes with a prefixes referring to France overseas territories

# Commune prefixes to delete
delete_communes <- c("97","98")

#Create mapping of cantons/communes dropping duplicate communes with NA value
#These are former communes, not assigned to any current canton
cantons_key <- cantons %>% 
  select(COM,CAN,LIBELLE) %>% 
  filter(!str_extract(COM, "^..") %in% delete_communes) %>%
  group_by(COM) %>%
  arrange(CAN) %>%
  filter(row_number()==1) %>%
  ungroup()

# 2. Deal with NAs in mapping data ----------------------------------------

# Commune definitions change frequently, with communes merged, split, renamed or
# renumbered. This code links outdated commune codes to their respective canton in
# the 2023 data.

#Find communes which have been renumbered
cantons_events <- events %>% 
  select(COM_AV,DATE_EFF,LIBELLE_AV,COM_AP,LIBELLE_AP,TYPECOM_AP) %>% 
  filter(!(COM_AV==COM_AP)) %>% # old commune number!= new commune number
  filter(TYPECOM_AP=="COM") %>% #only interested in changes creating a canton, not delegated canton
  arrange(COM_AV,desc(DATE_EFF)) %>% #Order by date and keep most recent change
  distinct(COM_AV, .keep_all=TRUE)

#Create a dataframe containing all communes with no assigned canton
na_cantons <- cantons_key %>% 
  filter(is.na(CAN)) %>% 
  filter(!(str_detect(LIBELLE,"Arrondissement"))) #These are large cities with labels not numbers

#Some communes have changed name and/or number multiple times
#Identify these cantons and the final commune and canton code

vector_AP <- cantons_events$COM_AP #list of original commune numbers
vector_AV <- cantons_events$COM_AV #list of commune numbers after a change

#Create vector containing the old and new commune names
named_vector <- setNames(vector_AP ,vector_AV)

#Create dataframe with original and final commune names
# It appears no commune changes name more than twice, with
# exception of communes that switch back and forth between 2 commune numbers
# Mutate statement searches for communes with missing cantons in na_cantons in the
# event data before changes (COM_AV). If there is a match it replaces it with the 
# new value from COM_AP (value after change). This is done twice.
changeCantons <- na_cantons %>% 
  mutate(Change1=if_else(COM %in% vector_AV,named_vector[COM],NA)) %>% 
  mutate(Change2=if_else(Change1 %in% vector_AV,named_vector[Change1],NA)) %>% 
  mutate(Change2=if_else(Change2==COM,NA,Change2)) %>% 
  #mutate(Change3=if_else(Change2 %in% vector_AV,named_vector[Change2],NA)) %>% 
  mutate(finalCOM=if_else(is.na(Change2),Change1,Change2)) %>% 
  select(COM,LIBELLE2=LIBELLE,finalCOM)

#Add cantons to the NA data
nas_corrected <- left_join(changeCantons,cantons_key,by=c("finalCOM"="COM")) %>% 
  drop_na() %>%  #only 5 entries that are not in Paris or Lyon
  select(COM,CAN,LIBELLE)

#Create dataframe of all the canton/commune data without unassigned canton NAS
# Includes NAS associated with the large cities of Lyon and Paris as what to
# keep labels for these communes but don't have cantons
cantons_no_nas <- cantons_key %>% 
  filter(!is.na(CAN)|(str_detect(LIBELLE,"Arrondissement")))

#Add df of corrected NAs to df of communes/cantons without NAs
#The only NAs in this df are from Lyon and Paris
final_conversion <- rbind(cantons_no_nas,nas_corrected) 
# 3. Load Geo Data on Establishments --------------------------------------

# Create SQLite database to easily access large csv
mydb <- dbConnect(SQLite(),"SQL Database//Establishments.sqlite3")

#Gives column names from table in SQLite database
#dbListFields(mydb, "Establishments2")

#Deletes existing table in SQLite database
#DBI::dbRemoveTable(mydb, "Establishments2")

#Read key columns of establsihment into database in chunks specifying column type

read_csv_chunked("Raw Data//Sirene Data//Establishments 2.csv", 
                 callback = function(chunk, dummy){
                   dbWriteTable(mydb, "Establishments2", chunk, append = T)}, 
                 chunk_size = 10000, col_names = TRUE, col_types=
                   cols_only(siren="i",
                             nic="c",
                             dateCreationEtablissement="c", #col_date(format = "AAAA-MM-JJ"),
                             codePostalEtablissement="c",
                             codeCommuneEtablissement="c",
                             libelleCommuneEtablissement="c",
                             coordonneeLambertAbscisseEtablissement="d",
                             coordonneeLambertOrdonneeEtablissement="d",
                             activitePrincipaleRegistreMetiersEtablissement="c",
                             nomenclatureActivitePrincipaleEtablissement="c"))

#Create table from Database
establishmentdb2 <- tbl(mydb, "Establishments2")

test<- establishmentdb2 %>% 
  head(1000) %>% 
  collect() # Saves dataframe so not only stored in memory

str(test)

# Filter dataframe to include only agriculture related legal entities (identified through sirens) 
# This reduces the volume of data to be linked
location_data <- establishmentdb2 %>% 
  filter(siren %in% enterprise_vector) %>% 
  collect() # Saves dataframe so not only stored in memory

# List of postcodes to delete as in French Overseas Territories
# https://en.wikipedia.org/wiki/Postal_codes_in_France#:~:text=Overseas%20D%C3%A9partements%20and%20Territories%20use,%2C%20988%20(New%20Caledonia).
# Keep as character not numeric to retain leading zeroes
delete_postcodes <- c("971","972", "973","974","975","976","984","986","987","988")

location_data_cleaned <- location_data %>% 
  mutate(siret=paste(siren,nic,sep="")) %>% #Combine nic and siren into siret
  select(siret,siren,nic,dateCreationEtablissement,codePostalEtablissement,libelleCommuneEtablissement,
         codeCommuneEtablissement) %>% 
  filter(!str_extract(codePostalEtablissement, "^...") %in% delete_postcodes) %>% 
  filter(!str_extract(codeCommuneEtablissement, "^..") %in% delete_communes) #delete overseas communes

canton_data <- left_join(location_data_cleaned ,final_conversion,by=c("codeCommuneEtablissement"="COM"))
write_csv(canton_data,"Cleaned Data//Geographic Enterprise Data 3.csv")
