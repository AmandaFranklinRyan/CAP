library(tidyverse)
library(plotly)
library(sf)

data_2021 <- read_csv("Arable Land 2021 Corrected.csv")
boundaries <- st_read("Cleaned Data//Boundaries Corrected//Boundaries 2023 Corrected.shp")
data_all <- read_csv("Final Datasets//Arable Data 2007-2022 Corrected NAs.csv")

#Which crops cover the greatest area of land?
barchart_df <- data_2021 %>% 
  group_by(description) %>% 
  summarise(totalArea=sum(Area)) %>% 
  ungroup() %>% 
  arrange(desc(totalArea)) %>% 
  head(15)

arable_barchart <- plot_ly(
  data = barchart_df,
  x = ~totalArea,
  y = ~fct_reorder(description,totalArea),
  type = 'bar',
  orientation = 'h') %>% 
  layout(title = "Area covered by 15 most common crops (2021)",
         yaxis = list(title = ""),
         xaxis = list(title = "Area (hectares)"))
#Grass/fallow land, Winter wheat and corn are the most common

#Export list of crops in 2014 data
crops_list <- data_2014 %>% 
  distinct(description)

write_csv(crops_list,"Cleaned Data//Crops List 2014.csv")

# 2. Plot Wine growing regions --------------------------------------------

all_crops_df <- arable_2022%>% 
  select(description) %>% 
  distinct()

wine_vector <- c("Vigne : raisins de cuve","Vigne : raisins de cuve non en production",
   "Vigne : raisins de table")  

wine_df <- arable_2022 %>% 
  filter(description == "VIGNES")

#Clean boundary data for joining

delete_communes <- c("97","98") #Overseas territories

boundaries_key <- boundaries %>% 
  filter(year==2023) %>% 
  group_by(can_name_lo) %>% 
  filter(n()>1) %>%
  ungroup()
  distinct(can_name_lo)
  mutate(canton=str_replace_all(can_code, "[\\[\\]'\\]]", "")) %>% 
  filter(!str_extract(canton, "^..") %in% delete_communes) %>%
  select(can_name_lo,can_name_up)
  
#Join vineyards with canton boundaries
wine_geo <- left_join(boundaries_key,wine_df,by=c("can_name_lo"="Canton")) %>% 
  st_as_sf()

#Establish breaks for plotting number of farms/cantons
histogram_area <- wine_geo %>%
  plot_ly(x = ~Area, type = "histogram", 
          marker = list(color = 'blue', line = list(color = 'black', width = 1))) 

# Plot simple regional map for 2022
ggplot(wine_geo) +
  geom_sf(aes(fill = Area), linewidth = 0.001, alpha=0.9) +
  theme_void()+
  scale_fill_distiller(
    palette = "Reds",
    trans = c("log","reverse"), breaks = c(1, 50, 500,5000,10000,15000),
    name = "Area of Farmland (hectares)",
    na.value="#FEFBEA",
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      label.position = "bottom",
      title.position = "top",
      nrow = 1))+
  labs(title = "Vineyards in France (2022)",
       subtitle = "Area of vineyards per canton")+
  theme(
    legend.position = c(0.3, 0.09)
  )

# 2. Plot Wine growing regions for all years --------------------------------------------

#Clean boundary data for joining

delete_communes <- c("97","98") #Overseas territories

boundaries_key <- boundaries %>% 
  mutate(CantonCode=str_replace_all(can_cod, "[\\[\\]'\\]]", "")) %>% 
  filter(!str_extract(CantonCode, "^..") %in% delete_communes) %>%
  select(cn_nm_l,CantonCode)

# Create list of years for loop
year_list <- data_all %>% 
  select(Year) %>% 
  distinct() %>% 
  pull()

#Select wine data only
wine_df <- data_all %>% 
  filter(Description == "VIGNES")

wine_geo <- inner_join(boundaries_key,wine_df,by=c("CantonCode"="CantonCode")) %>% 
  st_as_sf()

for (year in year_list){
  
  current_df <- wine_geo %>% 
    filter(Year==year)
  
  #Establish breaks for plotting number of farms/cantons
  #histogram_area <- wine_geo %>%
    #plot_ly(x = ~Area, type = "histogram", 
            #marker = list(color = 'blue', line = list(color = 'black', width = 1))) 
  
  # Plot simple regional map for 2022
  current_plot <- ggplot(current_df) +
    geom_sf(aes(fill = Area), linewidth = 0.001, alpha=0.9) +
    theme_void()+
    scale_fill_distiller(
      palette = "Reds",
      trans = c("log","reverse"), breaks = c(1, 50, 500,5000,10000,15000),
      name = "Area of Farmland (hectares)",
      na.value="#FEFBEA",
      guide = guide_legend(
        keyheight = unit(3, units = "mm"),
        keywidth = unit(12, units = "mm"),
        label.position = "bottom",
        title.position = "top",
        nrow = 1))+
    labs(title =paste("Vineyards in France",year,sep=""),
         subtitle = "Area of vineyards per canton")+
    theme(
      legend.position = c(0.3, 0.09)
    )
  
  current_file <- paste("Visualisations\\Vineyards",as.character(year),".png",sep="")
  
  ggsave(current_file, plot = current_plot, width = 6, height = 4)
  
}





