library(tidyverse)
library(USAboundaries)
library(sf)
library(leaflet)


#===========================================================================================
# Tables
#===========================================================================================

# Create a table to show US Totals
datatable(health_data_tidy %>%
            filter(Place == "U.S. Total"))

datatable(health_data_tidy %>% 
            select(`Indicator Category`, `Shortened Indicator Name`) %>%
            unique())




#============================================================================================
# Charts
#============================================================================================


# Opioid Motrality Rate

health_data_tidy %>%
  filter(`Indicator Category` == "Behavioral Health/Substance Abuse",
         `Shortened Indicator Name` == "Opioid-Related Overdose Mortality Rate") %>%
  ggplot(aes(Year, Value, group = City)) +
  geom_line(aes(color = City)) +
  theme_bw() 




# Disease related mortality rate

health_data_tidy %>%
  filter(`Health Type` == "Disease Related",
         `Shortened Indicator Name` %in% c("All Types of Cancer Mortality Rate",
                                           "Diabetes Mortality Rate",
                                           "Heart Disease Mortality Rate",
                                           "HIV-Related Mortality Rate",
                                           "Infant Mortality Rate",
                                           "Lung Cancer Mortality Rate",
                                           "Opioid-Related Overdose Mortality Rate",
                                           "Pneumonia & Influenza Mortality Rate")) %>%
  group_by(Year, `Shortened Indicator Name`, City, State) %>%
  summarise(Value = mean(Value)) %>%
  ggplot(aes(Year, Value, group = `Shortened Indicator Name`)) +
  geom_line(aes(color = `Shortened Indicator Name`)) +
  facet_wrap(~ City) + 
  theme_bw() +
  scale_color_brewer(palette = "Spectral") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  


# Map of 2011 & 2013 heart disease

health_data_tidy_geo %>%
  leaflet() %>%
  setView(lng = -100, lat = 42.3601, zoom = 3) %>%
  addTiles() %>%
  addCircles(data = health_data_tidy_geo %>%
               filter(`Shortened Indicator Name` == "Heart Disease Mortality Rate",
                      Year == 2011), ~lon, ~lat, 
             weight = 1, radius = ~Value*1000) %>%
  addCircles(data = health_data_tidy_geo %>%
               filter(`Shortened Indicator Name` == "Heart Disease Mortality Rate",
                      Year == 2013), ~lon, ~lat, 
             weight = 1, radius = ~Value*1000, color = "red")


# Heart disease mortality

health_data_tidy_geo %>%
  filter(`Shortened Indicator Name` == "Heart Disease Mortality Rate") %>%
  ggplot(aes(Year, Value, color = City)) +
  geom_line(size = 1.5) +
  theme_bw() +
  theme(legend.position = "top")




