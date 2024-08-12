library(tidyverse)
library(DBI) #fpr connecting to SQLite
library(RSQLite) #fpr connecting to SQLite
library(readr)
library(data.table)
library(bit64)

head_siren <- read.csv("Cleaned Data//Siren Data Ungrouped.csv",nrows=1000)

str(head_siren)

# 1. Load first 1000 rows of Establishments Data to see column names -----------------------------------

head_establishments <- read.csv("Raw Data//Sirene Data//Establishments 2.csv",nrows=1000)
colnames(head_establishments)
# Interested in :	anneeEffectifsEtablissement and trancheEffectifsEtablissement

#Import list of all agriculture related enterprises from code file:
enterprise_list <- read_csv("Final Datasets//Agricultural Enterprises List.csv")
enterprise_vector <- as.integer(enterprise_list$siren)

# 2. Select variables of interest from Establishments file -------------------

# Create SQLite database to easily access large csv
mydb <- dbConnect(SQLite(),"SQL Database//Establishments Employees Data.sqlite3")

read_csv_chunked("Raw Data//Sirene Data//Establishments 2.csv", 
                 callback = function(chunk, dummy){
                   dbWriteTable(mydb, "Establishments Employees Data", chunk, append = T)}, 
                 chunk_size = 10000, col_names = TRUE, col_types=
                   cols_only(siren="i",
                             siret="c",
                             anneeEffectifsEtablissement="c", #col_date(format = "AAAA-MM-JJ"),
                             trancheEffectifsEtablissement="c"))

#dbDisconnect(mydb)

#Create table from Database
establishment_extra <- tbl(mydb, "Establishments Employees Data")

number_employees <- establishment_extra %>% 
  filter(siren %in% enterprise_vector) %>% #select only agriculture related businesses
  collect()

table(number_employees$trancheEffectifsEtablissement)

cleaned_employee_df <- number_employees %>% 
  select(-anneeEffectifsEtablissement)

table(cleaned_employee_df$Year)
#Only year mentioned in data is 2021

#Calculate fraction of NAs
nas_df <- cleaned_employee_df %>% 
  filter(is.na(Year)& is.na(trancheEffectifsEtablissement)) %>% 
  distinct(siren) %>% 
  nrow()

(nas_df/nrow(enterprise_list))*100
#95% of agricultural enterprises have NAs in both categories

# 3. Extract extra variables from Legal file ------------------------------
# Downloaded from: https://www.data.gouv.fr/fr/datasets/base-sirene-des-entreprises-et-de-leurs-etablissements-siren-siret/
head_legal <- read.csv("Raw Data//Sirene Data//StockUniteLegale_utf8.csv",nrows=1000)
colnames(head_legal)

# Create SQLite database to easily access large csv
mydb <- dbConnect(SQLite(),"SQL Database//Establishments Company Size.sqlite3")

read_csv_chunked("Raw Data//Sirene Data//StockUniteLegale_utf8.csv", 
                 callback = function(chunk, dummy){
                   dbWriteTable(mydb, "Establishments Company Size", chunk, append = T)}, 
                 chunk_size = 10000, col_names = TRUE, col_types=
                   cols_only(siren="i",
                             anneeCategorieEntreprise="c", #col_date(format = "AAAA-MM-JJ"),
                             categorieEntreprise="c"))

#Create table from Database
establishment_company <- tbl(mydb, "Establishments Company Size")

#Collect agricultural enterprises only
company_df <- establishment_company %>% 
  filter(siren %in% enterprise_vector) %>% #select only agriculture related businesses
  collect()

#Change data type
cleaned_company_df <- company_df %>% 
  select(-anneeCategorieEntreprise)

table(cleaned_company_df$Year)
#Only year mentioned in data is 2021

table(cleaned_company_df$categorieEntreprise)
#ETI    GE   PME 
#338    98 79773 

#Calculate fraction of NAs
nas_df_company <- cleaned_company_df %>% 
  filter(is.na(Year)& is.na(categorieEntreprise)) %>% 
  distinct(siren) %>% 
  nrow()

(nas_df_company/nrow(enterprise_list))*100
#95% of agricultural enterprises have NAs in both categories

# 3. Add extra columns to Sirene Data -------------------------------------

#Select most recent value of number of employees variable
# The business size only exists at unit level not establishment level

extra_data <- left_join(cleaned_employee_df,cleaned_company_df) %>% 
  rename(EmployeesNumber=trancheEffectifsEtablissement,
         BusinessSize=categorieEntreprise) %>% 
  mutate(siret=as.integer64(siret)) %>% 
  as.data.table()

str(extra_data)

sirene_df <- fread("Cleaned Data//Siren Data Ungrouped.csv")

sirene_extra <-extra_data[sirene_df, on = "siret", nomatch = NA]

sirene_extra_df <- as.data.frame(sirene_extra)
write_csv()

sirene_extra_d
