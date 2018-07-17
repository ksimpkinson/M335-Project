library(tidyverse)
library(dygraphs)
library(DT)
library(stringr)
library(sf)
library(USAboundaries)
library(knitr)
library(leaflet)
library(ggthemes)
library(ggridges)


## @knitr data_tidying

#========================================================================================
# Load data
#========================================================================================

# Import health data
health_data <- read_csv("../Big City Data/Big City.csv")

# Import geo data
us_cities <- us_cities() %>%
  rename(City = "city",
         State = "state_abbr")



#========================================================================================
# Clean up data
#========================================================================================

health_data_tidy <- health_data %>%
  
  # Include all races and both genders
  filter(Sex == "Both",
         `Race/Ethnicity` == "All") %>%
  
  # Exclude unused columns
  select(-c(Indicator, `Shortened Indicator Name (Graph)`,
            `BCHC Requested Methodology`, Source, Methods, 
            Notes, `90% Confidence Level - High`,
            `90% Confidence Level - Low`,
            `95% Confidence Level - High`,
            `95% Confidence Level - Low`)) %>%
  
  # Add a health category based on type
  mutate(Value = parse_number(Value),
         `Health Type` = case_when(
           `Shortened Indicator Name` %in% c("AIDS Diagnoses Rate",
                                             "All Types of Cancer Mortality Rate",
                                             "Asthma Emergency Department Visit Rate",
                                             "Death Rate (Overall)",
                                             "Diabetes Mortality Rate",
                                             "E.coli Infections",
                                             "Heart Disease Mortality Rate",
                                             "HIV-Related Mortality Rate",
                                             "HIV Diagnoses Rate",
                                             "Infant Mortality Rate",
                                             "Life Expectancy",
                                             "Low Birth Weight Babies",
                                             "Lung Cancer Mortality Rate",
                                             "Opioid-Related Overdose Mortality Rate",
                                             "Persons Living with HIV/AIDS Rate",
                                             "Pneumonia & Influenza Mortality Rate",
                                             "Salmonella Infections",
                                             "Tuberculosis Incidence Rate")               ~  "Disease Related",
           `Shortened Indicator Name` %in% c("Adult Binge Drinking",
                                             "Adult Obesity",
                                             "Adult Physical Activity Levels",
                                             "Adult Seasonal Flu Vaccine",
                                             "Adult Smoking",
                                             "Child Seasonal Flu Vaccine",
                                             "Pneumonia Vaccine (Age 65+)",
                                             "Population Uninsured",
                                             "Preschool Enrollment",
                                             "Teen Binge Drinking",
                                             "Teen Obesity",
                                             "Teen Physical Activity Levels",
                                             "Teen Smoking",
                                             "Unemployment")                              ~  "Personal Decisions",
           `Shortened Indicator Name` %in% c("200% Below the Poverty Level",
                                             "Adult Population",
                                             "Aging Population",
                                             "Child Poverty",
                                             "Children's Blood Lead Levels",
                                             "English Speaking Population",
                                             "Excessive Housing Cost Burden",
                                             "Firearm-Related ED Visit Rate",
                                             "Firearm-Related Mortality Rate",
                                             "Foreign Born Population",
                                             "High School Graduation",
                                             "Homicide Rate",
                                             "Median Household Income",
                                             "Motor Vehicle Mortality Rate",
                                             "Spanish Speaking Population",
                                             "Suicide Rate",
                                             "Teen Population",
                                             "Total Population",
                                             "Youth Population")                          ~  "Uncontrollable/Other"
         )) %>%
  
  # Split place into city and state
  separate(Place, into = c("City", "State"), sep = ", ") %>%
  
  # Split city farther to exclude extra information
  separate(City, into = c("City", "Location Description"), sep = " \\(") %>%
  
  # Fix San Diego County to be just San Diego
  mutate(City = case_when(
    City == "San Diego County"    ~  "San Diego",
    TRUE                          ~  City
  ))


#========================================================================================
# Join data
#========================================================================================

# Join health data with geo data
health_data_tidy_geo <- left_join(us_cities, health_data_tidy)

# Extract longitude and latitude from data
health_data_tidy_geo <- health_data_tidy_geo %>%
  filter(!is.na(`Indicator Category`)) %>%
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))
  
