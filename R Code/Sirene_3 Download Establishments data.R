library(tidyverse)
library(DBI) #fpr connecting to SQLite
library(RSQLite) #fpr connecting to SQLite
library(sf) #for reading spatial dataframes


# 1. Collate all geographic Data ------------------------------------------

#Import commune/canton boundaries data from: https://public.opendatasoft.com/explore/dataset/georef-france-canton/table/?disjunctive.reg_name&disjunctive.dep_name&disjunctive.arrdep_name&disjunctive.can_name&sort=year
boundaries <- st_read(dsn="Raw Data\\Boundaries 2023","georef-france-canton-millesime")

#Postcode data from: https://www.data.gouv.fr/fr/datasets/base-officielle-des-codes-postaux/
postcodes <- read_delim("Raw data//French Postcodes.csv", delim=";", locale=locale(encoding="latin1"))

#Drop unnecessary geographic data
postcodes_key <- postcodes %>% 
  select(-Ligne_5,-Libellé_d_acheminement,-Nom_de_la_commune) %>% 
  distinct()

#Convert boundaries data to dataframe
boundaries_df <- boundaries %>% 
  as.data.frame() %>% 
  filter(year==2023) %>% #most recent boundaries
  select(can_code,can_name_lo,can_burcent)  

#Combine boundaries and postcodes
geo_combined <- left_join(boundaries_df,postcodes_key, by=c("can_burcent"="#Code_commune_INSEE"))

# Create SQLite database to easily access large csv
mydb <- dbConnect(SQLite(),"SQL Database//Establishments.sqlite3")

#Gives column names from table in SQLite database
#dbListFields(mydb, "Establishments2")

#Deletes existing table in SQLite database
#DBI::dbRemoveTable(mydb, "Establishments2")

#Read csv into database in chunks
read_csv_chunked("Raw Data//Establishments 2.csv", 
                 callback = function(chunk, dummy){
                 dbWriteTable(mydb, "Establishments2", chunk, append = T)}, 
                 chunk_size = 10000, col_names = TRUE, col_types=
                   cols_only(siren="c",
                             nic="c",
                             dateCreationEtablissement="c", #col_date(format = "AAAA-MM-JJ"),
                             codePostalEtablissement="c",
                             codeCommuneEtablissement="c",
                             libelleCommuneEtablissement="c",
                             coordonneeLambertAbscisseEtablissement="d",
                             coordonneeLambertOrdonneeEtablissement="d",
                             activitePrincipaleRegistreMetiersEtablissement="c",
                             nomenclatureActivitePrincipaleEtablissement="c"))
                             
establishmentdb2 <- tbl(mydb, "Establishments2")

colnames(geo_combined)

location_data <- establishmentdb2 %>% 
  #filter(str_starts(activitePrincipaleEtablissement,"01")) %>% 
  head(10000) %>% 
  collect() # Saves dataframe so not only stored in memory

location_data_clean <- location_data %>% 
  mutate(dateCreationEtablissement=as.Date(dateCreationEtablissement,format="%Y-%m-%d")) %>% 
  mutate(creationYear=year(dateCreationEtablissement)) %>% 
  select(-dateCreationEtablissement)

geo_df <- left_join(location_data_clean,geo_combined,by=c("codePostalEtablissement"="Code_postal"))
  