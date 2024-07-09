  library(tidyverse)
  library(DBI) #fpr connecting to SQLite
  library(RSQLite) #fpr connecting to SQLite
  library(sf) #for reading spatial dataframes
  
  
  # 1. Collate all geographic Data ------------------------------------------
  
  enterprise_list <- read_csv("Cleaned Data//Agricultural Enterprises List.csv")
  enterprise_vector <- as.character(enterprise_list$siren)
  
  #Import commune/canton boundaries data from: https://public.opendatasoft.com/explore/dataset/georef-france-canton/table/?disjunctive.reg_name&disjunctive.dep_name&disjunctive.arrdep_name&disjunctive.can_name&sort=year
  boundaries <- st_read(dsn="Raw Data\\oundaries 2023","georef-france-canton-millesime")
  
  #Import commune/canton data from: https://www.insee.fr/fr/information/6800675
  cantons <- read_csv("Raw Data//Commune Data.csv")
  
  #Download event data from: https://www.insee.fr/fr/information/6800675
  events <- read_csv("Raw Data//Event Data.csv")
  
  
  delete_communes <- c("97","98","69")
  
  cantons_key <- cantons %>% 
    select(COM,CAN,LIBELLE) %>% 
    filter(!str_extract(COM, "^..") %in% delete_communes) %>%
    group_by(COM) %>%
    arrange(CAN) %>%
    filter(row_number()==1) %>%
    ungroup()
    #drop foreign communes
    #filter(!is.na(CAN)| (str_starts(COM,"751")|str_starts(COM,"69"))) #Drop all NAs unless the commune is Paris
                                               # This retains Paris labels but drops old cantons
  
  cantons_events <- events %>% 
    select(COM_AV,DATE_EFF,LIBELLE_AV,COM_AP,LIBELLE_AP,TYPECOM_AP) %>% #find communes which have been renumbered
    filter(!(COM_AV==COM_AP)) %>% 
    filter(TYPECOM_AP=="COM") %>% 
    arrange(COM_AV,desc(DATE_EFF)) %>% #Order by date and keep most recent change
    distinct(COM_AV, .keep_all=TRUE)
    
  
  #Create a dataframe containing all cantons with NAs
  na_cantons <- cantons_key %>% 
    filter(is.na(CAN)) %>% 
    filter(!(str_detect(LIBELLE,"Arrondissement"))) #These are large cities with labels not numbers
  
  #Some cantons have changed name and/or numbermultiple times
  vector_AP <- cantons_events$COM_AP
  vector_AV <- cantons_events$COM_AV
  
  #Create vector containing the old and new commune names
  named_vector <- setNames(vector_AP ,vector_AV)
  
  named_vector["91390"]
  
  changeCantons <- na_cantons %>% 
    mutate(Change1=if_else(COM %in% vector_AV,named_vector[COM],NA)) %>% 
    mutate(Change2=if_else(Change1 %in% vector_AV,named_vector[Change1],NA)) %>% 
    mutate(Change2=if_else(Change2==COM,NA,Change2)) %>% 
    #mutate(Change3=if_else(Change2 %in% vector_AV,named_vector[Change2],NA)) %>% 
    mutate(finalCOM=if_else(is.na(Change2),Change1,Change2)) %>% 
    select(COM,LIBELLE2=LIBELLE,finalCOM)
  
  nas_corrected <- left_join(changeCantons,cantons_key,by=c("finalCOM"="COM")) %>% 
    drop_na() #only 5 entries
  
  nas_formatted <- nas_corrected %>%
    select(COM,CAN,LIBELLE)
  
  final_conversion <- rbind(cantons_key,nas_formatted) %>% 
    drop_na()
    
  
    
  
  
  
  cantons_events2 <- left_join(na_cantons2,events,by=c("COM"="COM_AV")) %>% 
    select(COM,DATE_EFF,LIBELLE_AV,COM_AP,LIBELLE_AP) %>% #find communes which have been renumbered
    filter(!(COM==COM_AP)) %>% 
    arrange(COM,desc(DATE_EFF)) %>% #Order by date and keep most recent change
    distinct(COM, .keep_all=TRUE)
  
  
  cantons_no_nas <- cantons_key %>% 
    drop_na()
   
  cantons_corrected2 <- left_join(cantons_events2,cantons_no_nas, by=c("COM_AP"="COM"))
  
  nas_cantons_corrected2 <- cantons_corrected2 %>% 
    select(COM=COM_AP, CAN, LIBELLE) %>% 
    filter(is.na(CAN)) %>% 
    distinct(COM)
  
  second_correction <- left_join(nas_cantons_corrected2,events,by=c("COM"="COM_AV")) %>% 
    select(COM,DATE_EFF,LIBELLE_AV,COM_AP,LIBELLE_AP) %>% #find communes which have been renumbered
    filter(!(COM==COM_AP)) %>% 
    arrange(COM,desc(DATE_EFF)) %>% #Order by date and keep most recent change
    distinct(COM, .keep_all=TRUE)
  
  cantons_corrected3 <- left_join(second_correction,cantons_no_nas, by=c("COM_AP"="COM"))
    
    left_join
  
  #Join communes and events tables to see if these are old communes which have changed names
  cantons_events <- left_join(na_cantons,events,by=c("COM"="COM_AV")) %>% 
    select(COM,DATE_EFF,COM_AP,LIBELLE_AP) %>% 
    filter(!(COM==COM_AP)) %>% #Find cantons which have changed name
    arrange(COM,desc(DATE_EFF)) %>% 
    distinct(COM, .keep_all=TRUE) #Arrange in date order and select commune name with mosr recent change
  
  #Some cantons have changed name and/or numbermultiple times
  vector_AP <- cantons_events$COM_AP
  vector_AV <- cantons_events$COM
  
  #Create vector containing the old and new commune names
  named_vector <- setNames(vector_AP ,vector_AV)
  
  #Create new field with the most recent commune name
  multiple_changes <- cantons_events %>% 
    mutate(commueNew=if_else(COM_AP %in% COM,named_vector[COM_AP],COM_AP)) %>% 
    select(-LIBELLE_AP)
  
  cantons_corrected <- left_join(cantons_events,cantons_no_nas, by=c("COM_AP"="COM")) %>% 
    filter(!(LIBELLE_AV==LIBELLE)) %>% 
    filter((LIBELLE_AP==LIBELLE)) %>% 
    arrange(COM,desc(DATE_EFF)) %>% 
    distinct(COM, .keep_all=TRUE)
  
  
  
  #Some communes appear twice with NA as canton and different name label
  #Presumably this is a commune which has changed name
  
  #Identify duplicate communes and remove the outdated name
  
  #Create vector of duplicates
  communes_duplicated <- cantons_key %>% 
    group_by(COM) %>% 
    summarise(Total=n()) %>% 
    filter(Total>1) %>% 
    ungroup() %>% 
    select(COM) %>% 
    pull()
  
  cantons_corrected <- cantons_key %>% 
    filter(!(is.na(CAN) & (COM %in% communes_duplicated)))
  
  length(unique(cantons_key$CAN))
  
  #Merged cantons or changed names
  merged_communes <- events %>% 
    #filter(TYPECOM_AP=="COMA") %>% 
    select(MOD,DATE_EFF,COM_AV,COM_AP,LIBELLE_AV,LIBELLE_AP) %>% 
    filter(!(COM_AV==COM_AP)) %>% 
    group_by(COM_AV) %>% 
    summarise(Total=n())
  
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
  
  # Filter dataframe to include only relevant legal entities (identified through sirens) 
  location_data <- establishmentdb2 %>% 
    filter(siren %in% enterprise_vector) %>% 
    collect() # Saves dataframe so not only stored in memory
  
  
  # List of postcodes to delete as in French Overseas Territories
  # https://en.wikipedia.org/wiki/Postal_codes_in_France#:~:text=Overseas%20D%C3%A9partements%20and%20Territories%20use,%2C%20988%20(New%20Caledonia).
  # Keep as character not numeric to retain leading zeroes
  delete_postcodes <- c("971","972", "973","974","975","976","984","986","987","988")
  
  location_data_cleaned <- location_data %>% 
    mutate(siret=paste(siren,nic,sep="")) %>% #Combine nic and siren into siret
    select(siret,dateCreationEtablissement,codePostalEtablissement,libelleCommuneEtablissement,
           codeCommuneEtablissement) %>% 
    filter(!str_extract(codePostalEtablissement, "^...") %in% delete_postcodes) %>% 
    filter(!str_extract(codeCommuneEtablissement, "^..") %in% delete_communes)
    #head(1000)
  
  colnames(location_data_cleaned)
  
  canton_data <- left_join(location_data_cleaned ,cantons_key,by=c("codeCommuneEtablissement"="COM"))
   
  test <- canton_data %>% 
