library(tidyverse)
library(DBI)
library(RSQLite)


#Import small data sample to ascertain data types
short_data <- read_csv("Raw Data//Sirene Data//Mini Establishment Data.csv")

# Create SQLite database to easily access large csv
mydb <- dbConnect(SQLite(),"SQL Database//Establishment History Total.sqlite3")

#Read csv into database in chunks
read_csv_chunked("Raw Data//Sirene Data//Establishment History.csv", 
                 callback = function(chunk, dummy){
                   dbWriteTable(mydb, "Establishment History Total", chunk, append = T)}, 
                 chunk_size = 10000, col_types = "cccccclclllllcclcl")

historydb <- tbl(mydb, "Establishment History Total")

agriculture <- historydb %>% 
  filter(activitePrincipaleEtablissement %LIKE% "01.%") %>% #select only agriculture related businesses
  collect()

yearInterest <- 2010
currentDate <- 2024

#Create new columns for start and end dates of establishments
agriculture_clean <- agriculture %>% 
  select(siren:dateDebut,activitePrincipaleEtablissement,nomenclatureActivitePrincipaleEtablissement) %>% 
  mutate(dateFin=as.Date(dateFin)) %>% 
  mutate(dateDebut=as.Date(dateDebut)) %>% 
  mutate(startYear=year(dateDebut)) %>% 
  mutate(endYear=year(dateFin)) %>% 
  filter(endYear>=yearInterest|is.na(endYear)) %>%  #Drop all enterprises whose activities stopped before year of interest
  filter(!is.na(startYear)) #Drop all entries with no start date

#Add time data to dataframe
time_df <- agriculture_clean %>%
  mutate(current_Date=2024) %>% 
  mutate(endYear=if_else(is.na(endYear),current_Date,endYear)) %>% #Add 2024 as end date for all NAs
  rowwise() %>% #Need for seq() command
  mutate(datesActive= list(seq(from = startYear, to = endYear))) #Make a list of years between start and end years

#Create separate rows for each year
time_df_long <- time_df %>% 
  unnest(datesActive) 

#Only look at rows after 2010 (period of interest)
filtered_df <- time_df_long %>% 
  filter(datesActive>=yearInterest)

#Drop unnecessary columns and export
agriculture_total <- filtered_df %>% 
  select(-dateDebut,-endYear,-dateFin,-startYear,-current_Date)

write_csv(agriculture_clean, "Cleaned Data//Agricultural Clean 2.csv")

#Create list of enterprises of interest to select relevant data in Enterprises dataset

all_enterprises <- agriculture_total %>% 
  select(siren) %>% 
  distinct()

write_csv(all_enterprises, "Agricultural Enterprises List.csv")
