library(tidyverse)
library(DBI)
library(RSQLite)
library(readxl)

# 1. Load large data file with geo and enterprise data-------------------------

# Create SQLite database to easily access large csv
mydb <- dbConnect(SQLite(),"SQL Database//Geographic Enterprise Data 5.sqlite3")

#Read csv into database in chunks
read_csv_chunked("Cleaned Data//Sirene Data//Geographic Data Enterprises Final.csv", 
                 callback = function(chunk, dummy){
                   dbWriteTable(mydb, "Geographic Enterprise Data 5", chunk, append = T)}, 
                 chunk_size = 10000) #col_types = "cccccclclllllcclcl")

agriculturedb <- tbl(mydb, "Geographic Enterprise Data 5")

#Load all agriculutural data
agriculture_total <- agriculturedb %>% 
  collect()

# 2. Add agricultural codes -----------------------------------------------

#Download all versions of the agricultural code from here: https://www.insee.fr/fr/information/2416409
naf_2 <- read_excel("Raw Data//NAF Codes//NAF Rev2.xls")
naf_1993 <- read_excel("Raw Data//NAF Codes//NAF 1993.xls")
naf <- read_excel("Raw Data//NAF Codes//NAF 2003.xlsx")
nap <- read_excel("Raw Data//NAF Codes//NAP.xls")

#Clean data and make column names consistent
naf2_cleaned <- naf_2 %>% 
  filter(str_starts(Code,"01")) %>% 
  select(Code,description=`Intitulés de la  NAF rév. 2, version finale`)

naf1993_cleaned <- naf_1993 %>% 
  filter(str_starts(`NAF 1993 -`,"01")) %>% 
  select(Code=`NAF 1993 -`,description93=`Niveau 700 - Liste des classes`)

naf_cleaned <- naf %>% 
  filter(str_starts(`NAF rév. 1`,"01")) %>% 
  select(Code=`NAF rév. 1`, description_naf=`Niveau 700 - Liste des classes`)

nap_cleaned <- nap %>% 
  filter(str_starts(`NAP600`,"01")) %>% 
  select(Code=`NAP600`,description_nap=`LIB_NAP600`)

#Create list of agricultural codes in the Enterprise data
distinct_codes <- agriculture_total %>% 
  select(activitePrincipaleEtablissement,nomenclatureActivitePrincipaleEtablissement) %>% 
  distinct() %>% 
  select(Code=activitePrincipaleEtablissement, Version=nomenclatureActivitePrincipaleEtablissement)

#Combine codes from enterprise list with codes from different time periods
combined_NAF2 <-left_join(distinct_codes,naf2_cleaned) 
combined_1993 <-left_join(combined_NAF2,naf1993_cleaned, by=c("Code"="Code")) 
combined_NAF <- left_join(combined_1993,naf_cleaned, by=c("Code"="Code"))
combined_NAP <- left_join(combined_NAF,nap_cleaned, by=c("Code"="Code")) 

#Combine codes into single column
codes_all <- combined_NAP %>% 
  mutate(description=case_when(Version=="NAF1993"~description93,
                               Version=="NAFRev1"~description_naf,
                               Version=="NAFRev2"~description,
                               Version=="NAP"~description_nap)) %>% 
  select(Code, Version,description) %>% 
  group_by(Code) %>% 
  filter(row_number()==1) %>%
  ungroup()

write_csv(codes_all,"Cleaned Data//Agriculture_conversion_list.csv")
codes_all <- read_csv("Cleaned Data//Agriculture_conversion_list.csv")

# Add groupings to codes (Several different versions of NAF used, so combine equivalent livestock coes)

codes_all <- read_csv("Cleaned Data//Sirene Data//Agriculture_conversion_list.csv")

codes_grouped <- codes_all %>% 
  mutate(description=case_when(description=="Elevage de bovins associé à d'autres activités agricoles"~"Elevage de bovins",
                       (description=="Activités de soutien à la production animale"|description=="Services annexes à l'élevage")~"Services effectués au profit de l'élevage",
                       (description=="Chasse et piégeage"|description=="Chasse, piégeage et services annexes")~"Chasse",
                       description=="Aviculture"~"Élevage de volailles",
                       TRUE~ description))

codes_grouped <- write_csv(codes_grouped,"Cleaned Data//Agriculture_conversion_list_grouped.csv")

# 4. Summarise agricultural data ------------------------------------------

grouped_data <- agriculture_total %>% 
  group_by(datesActive,communeCode,cantonCode,activitePrincipaleEtablissement) %>% 
  summarise(number_Establishments=n())

grouped_code <- left_join(grouped_data,codes_all,by=c("activitePrincipaleEtablissement"="Code"))
write_csv(grouped_code,"Cleaned Data//Siren Data summarised.csv")

ungrouped_code <- left_join(agriculture_total,codes_grouped,by=c("activitePrincipaleEtablissement"="Code"))
write_csv(ungrouped_code,"Cleaned Data//Siren Data Ungrouped Final 3.csv")


