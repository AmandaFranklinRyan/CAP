library(tidyverse)
library(sf)
library(plotly)

#Data analysis on commune names not labels so Lyon and Paris are missing
# Have made assumption that if no end date appears in Establishments History df
# then the establishment is still operating

# 1. Load spatial and agricultural data -----------------------------------

siren_data <- read_csv("Cleaned Data//Siren Data summarised.csv")
boundaries <- st_read(dsn="Raw Data\\Boundaries 2023","georef-france-canton-millesime")
codes <- read_csv("Cleaned Data//Agriculture_conversion_list.csv")

#Select columns
cleaned_data <- siren_data %>% 
  select(-activitePrincipaleEtablissement,-Version,-communeCode) %>% 
  distinct() %>% 
  drop_na() #842 missing values

#What are the most common agricultural activities?
common_activities <- cleaned_data %>% 
  select(number_Establishments,description) %>% 
  group_by(description) %>% 
  summarise(Count=n()) %>% 
  arrange(desc(Count)) %>% 
  top_n(15)
#General categories and breeding are most common

# 2. Investigate breeding data --------------------------------------------

breeding_vector <- c("Élevage d'autres animaux","Élevage d'autres bovins et de buffles",
                    "Elevage de bovins","Élevage de vaches laitières",
                    "Élevage de chevaux et d'autres équidés","Élevage d'ovins et de caprins",
                    "Élevage de volailles","Activités de soutien à la production animale",
                    "Elevage d'ovins, caprins et équidés","Elevage de porcins",
                    "Aviculture","Elevage de bovins associé à d'autres activités agricoles",
                    "Elevage d'ovins, caprins, équins","Services effectués au profit de l'élevage",
                    "Élevage de chameaux et d'autres camélidés","Services annexes à l'élevage")

breeding_df <- cleaned_data %>% 
  filter(description %in% breeding_vector)

barchart_df <- breeding_df %>% 
  filter(datesActive==2024) %>% 
  group_by(description) %>% 
  summarise(`Number of Establishments`=n())

#Plot breeding types by number of establishments 
breeding_barchart <- plot_ly(
  data = barchart_df,
  x = ~`Number of Establishments`,
  y = ~fct_reorder(description,`Number of Establishments`),
  type = 'bar',
  orientation = 'h') %>% 
  layout(title = "French Breeding Data (2024)",
         yaxis = list(title = ""))

# 3. Plot map of dairy farming by canton --------------------------------------------

# Create df of dairy farming
dairy_df <- cleaned_data %>% 
  filter(description=="Élevage de vaches laitières") %>% 
  group_by(datesActive,cantonCode) %>% 
  summarise(number_Establishments=n()) %>% 
  ungroup() %>% 
  filter(datesActive==2024)

delete_communes <- c("97","98") #Overseas territories

#Format geo data to match format of dairy farming df
boundary_df <- boundaries %>% 
  mutate(canton=str_replace_all(can_code, "[\\[\\]'\\]]", "")) %>% #remove brackets from column
  filter(year==2024) %>% 
  filter(!str_extract(canton, "^..") %in% delete_communes) %>% #delete overseas territories
  select(canton,can_name_lo)

#Join datasets
dairy_geo <- left_join(boundary_df,dairy_df,by=c("canton"="cantonCode"))

#Look at 2024 data only

dairy_geo_2024_spatial <- dairy_geo %>% 
  select(geometry,number_Establishments) %>% 
  st_as_sf() #convert to spatial dataframe for plotting

#Establish breaks for plotting number of farms/cantons
histogram_nr_establishments <- dairy_geo_2024_spatial %>%
  plot_ly(x = ~number_Establishments, type = "histogram", 
          marker = list(color = 'blue', line = list(color = 'black', width = 1))) 
#Add breaks every 5 establishments

# Plot simple regional map for 2024
ggplot(dairy_geo_2024_spatial) +
  geom_sf(aes(fill = number_Establishments), linewidth = 0.2, alpha=0.9) +
  theme_void()+
  scale_fill_distiller(
    palette = "Greens",
    trans = c("reverse"), breaks = c(1, 5, 10, 15, 20, 25),
    name = "Number of Establishments",
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      label.position = "bottom",
      title.position = "top",
      nrow = 1))+
  labs(title = "Dairy Farming in France (2024)",
      subtitle = "Number of dairy farming enterprises per canton")+
  theme(
    legend.position = c(0.3, 0.09)
  )
  
# 4. Replicate Breeding Map on CD website by canton --------------------------------------------
# https://cdonline.articque.com/share/display/regions-elevages

#Select same categories as website
breeding_vector_2 <- c("Élevage d'autres animaux","Élevage d'autres bovins et de buffles",
"Élevage de chevaux et d'autres équidés","Élevage de vaches laitières",
"Élevage d'ovins et de caprins","Elevage de porcins","Élevage de volailles","Aviculture")

#Add colour scheme from webste
colour_palette <- c("Élevage de vaches laitières"="#9EC68C",
                    "Élevage d'autres bovins et de buffles"="#728C65",
                    "Élevage de chevaux et d'autres équidés"="#EFB77D",
                    "Élevage d'ovins et de caprins"="#FFFF59",
                    "Elevage de porcins"="#E4C5D4",
                    "Élevage de volailles"="#81B0DA",
                    "Élevage d'autres animaux"="#DE5959",
                    "Missing"="darkgrey")

#Plot pie charts of agricultural data for different years
pie_chart_df <- breeding_df %>% 
  filter(description %in% breeding_vector_2) %>% 
  mutate(description=if_else(description=="Aviculture","Élevage de volailles",description)) %>% 
  filter(datesActive==2024) %>% 
  group_by(description) %>% 
  summarise(Total=sum(number_Establishments)) %>% 
  ungroup() %>% 
  mutate(description=as.character(description))
  

pie_chart<- plot_ly(pie_chart_df, labels = ~description, values = ~Total, type = 'pie',
               textinfo = 'label+percent', insidetextorientation = 'radial',
               showlegend=FALSE,marker = list(colors = colour_palette[pie_chart$description])) %>% 
               layout(title = 'Agriculture Data (2024)')

#Create map of all breeding data by canton
breeding_summary_df <- breeding_df %>% 
  filter(description %in% breeding_vector_2) %>%
  mutate(description=if_else(description=="Aviculture","Élevage de volailles",description)) %>% 
  filter(datesActive==2024) %>% 
  group_by(cantonCode,description) %>% 
  summarise(number_establishments=n()) %>% 
  filter(number_establishments == max(number_establishments)) %>% # keep only description with highest number of establishments
  slice_max(number_establishments) %>% # In cantons with the same number of several farming types keep the first one
  ungroup() 

breeding_total <- left_join(boundary_df,breeding_summary_df,by=c("canton"="cantonCode")) %>%
  mutate(description=if_else(is.na(description),"Missing",description)) %>% 
  st_as_sf() #convert to spatial dataframe for plotting

agriculture_map <-ggplot()+
      theme_void()+
      geom_sf(data=breeding_total, aes(fill=description),color="black", size=0.1)+
      scale_fill_manual(values = colour_palette, name= "Breeding Type") +
      ggtitle("Livestock Breeding in France (2024)")

