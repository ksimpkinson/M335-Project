library(tidyverse)
library(dygraphs)
library(DT)


# Import health data
health_data <- read_csv("Big City Data/Big City.csv")


# Clean up data
# Include all races and both genders
# Exclude unused columns
health_data_tidy <- health_data %>%
  filter(Sex == "Both",
         `Race/Ethnicity` == "All") %>%
  select(-c(Indicator, `Shortened Indicator Name (Graph)`,
            `BCHC Requested Methodology`, Source, Methods, 
            Notes, `90% Confidence Level - High`,
            `90% Confidence Level - Low`,
            `95% Confidence Level - High`,
            `95% Confidence Level - Low`))


# Create a table to show US Totals
datatable(health_data_tidy %>%
            filter(Place == "U.S. Total"))

datatable(health_data_tidy %>% 
            select(`Indicator Category`, `Shortened Indicator Name`) %>%
            unique())
  
