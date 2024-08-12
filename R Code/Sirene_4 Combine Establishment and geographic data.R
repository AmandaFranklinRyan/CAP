library(tidyverse)

geo_data <- read_csv("Cleaned Data//Geographic Enterprise Data 3.csv") 
enterprise_data <- read_csv("Cleaned Data//Agricultural Data.csv")

#There are many missing values starting in 69, this is because the Metropolis of
# Lyon has no cantons. I have labelled them Metropolis.
# Paris doesn't have cantons either but these are already labelled with Arrondisements

geo_data_cleaned <- geo_data %>% 
  select(siren,nic,siret,postcode=codePostalEtablissement,labelEnterprise=libelleCommuneEtablissement,
         communeCode=codeCommuneEtablissement, cantonCode=CAN, labelGeo=LIBELLE) %>% 
  mutate(postcode=if_else(postcode=="[ND]",NA,postcode)) %>% 
  filter(!(is.na(postcode) & is.na(communeCode))) %>% #drop entries with no geographic information (836)
  mutate(labelGeo=if_else(str_detect(communeCode, "^69"), "Metropolis Lyon",labelGeo)) %>% 
  # Format siret column for joining to enterprise data
  mutate(siren=as.integer(siren)) %>% 
  mutate(nic=as.integer(nic)) %>% 
  #mutate(siret=paste(siren,nic,"")) %>% 
  #mutate(siret = str_replace_all(siret, " ", "")) %>% 
  #mutate(siret = as.numeric(siret)) %>% 
  select(-labelEnterprise,-siret)

#Remove exact duplicates from geo_data cleaned
geo_data_cleaned_distinct <- distinct(geo_data_cleaned)

#Combine geographic and establishment data and export
enterprise_geo <- left_join(enterprise_data,geo_data_cleaned_distinct,by=c("siren","nic"))
write_csv(enterprise_geo,"Cleaned Data//Geographic Data Enterprises Final.csv")
