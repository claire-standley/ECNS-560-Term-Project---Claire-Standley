
# Install necessary packages
#install.packages(c("tidyverse", "readxl", "janitor", "fixest", ))

# Library necessary packages
library(tidyverse)
library(readxl)
library(janitor)
library(fixest)



#Load the data

#farmers market food directory data (county-level)
farmers_raw <- read.csv("Farmers Market Data.csv") |>
  clean_names() #cleans variable name when loading the dataset in (underscores and such)

#census data (county level)
census_raw <-read_csv("USDA Census Dataset 2002-2022.csv") |>
  clean_names()

#agritourism law data set (state level)
laws_raw <- read_csv("Agritourism Law Implimentation.csv") |>
  clean_names()



#First converting variable formats, tidying, and removing duplicates

#cleaning the farmers market data
farmers_clean <- farmers_raw |> #creating a new data set with cleaned variables
  mutate(
    state = str_to_upper(state), #converting all states to uppercase letters for consistency
    county = str_to_upper(county), #converting counties to uppercase
    year = as.numeric(year) #storing all the years as numeric variables
  ) |>
  #remove empty rows
  filter(!is.na(state), !is.na(county), !is.na(year)) |>
  #remove duplicates
  distinct()

#Cleaning the law data
laws_clean <- laws_raw |>
  rename(
    adoption_year = year_of_first_adoption,
    state = x1,
    first_amend = x1st_amendment_year,
    second_amend = x2nd_amendment_year,
    third_amend = x3rd_amendment_year,
    fourth_amend = x4th_amendment_year
  ) |>
  mutate(
    state = str_to_upper(state),
    year = as.numeric(adoption_year)
  ) |>
  filter(!is.na(state), !is.na(year)) |> #removes rows with missing state/year
  distinct()

#Due to format of census data and overlapping in the variable county the data should
#be fixed by pivoting wide and giving each variable its own column

census_wide <- census_raw |>
  mutate(value_copy = value) |>
  #pivot to wide format
  pivot_wider(
    names_from = data_item,
    values_from = value_copy
  ) 


#cleaning the census data

# Columns storing "farm operations" for different year ranges
old_col <- "COMMODITY TOTALS, RETAIL, HUMAN CONSUMPTION - OPERATIONS WITH SALES"
mid_col <- "COMMODITY TOTALS, RETAIL, COMMUNITY SUPPORTED AG - OPERATIONS WITH SALES"
new_col <- "COMMODITY TOTALS, INCL VALUE-ADDED, RETAIL, DIRECTLY MARKETED, HUMAN CONSUMPTION - OPERATIONS WITH SALES"

#clean and combine into single variable
census_clean <- census_wide %>%
  mutate(
    state = str_to_upper(state),
    county = str_to_upper(county),
    year = as.numeric(year),
    
    # Clean suppressed values for all columns
    across(all_of(c(old_col, mid_col, new_col)),
           ~ na_if(., "(D)") %>% na_if("(Z)")),
    
    # Parse numbers
    across(all_of(c(old_col, mid_col, new_col)), parse_number),
    
    # Combine into single variable
    farms_value_added = coalesce(.data[[new_col]],
                                 .data[[mid_col]],
                                 .data[[old_col]]),
    
    # Replace remaining NA with 0 if needed
    farms_value_added = ifelse(is.na(farms_value_added), 0, farms_value_added)
  ) %>%
  distinct(state, county, year, .keep_all = TRUE)

#The primary key for the farmers and census datasets are state, county, and year (county level)

#The primary key for the laws data set is state and year (state level)

#Handling missing values

#check missingness
colSums(is.na(farmers_clean)) #farmers data
colSums(is.na(census_clean)) #census data
colSums(is.na(laws_clean)) #law data

#We have lots of missing values in the census data specifically all the observations contain
  #N/A values for week_ending, zip code, region, and watershed. As I am not going to be using
  #these variables I will leave them just as blanks in the dataset but to properly clean the 
  #data these variables may be dropped as they are meaningless due to fully blank entries.

#The farmers market data set also has lot of missing entries. Due to the entries being filled
  #out by the individuals hosting the farmers market themselves this is to be expected. The 
  #missing values again should not be relevant for this analysis and thus I will leave them. 


#Creating a county-level farmers market count
farmers_county <- farmers_clean |>
  group_by(state, county, year) |> #group data by location and year
  summarize(
    market_ops = n(), #count number of rows (markets)
    .groups = "drop" # removes grouping after summarize
  )

#Expand law data into panel
years <- tibble(year = min(farmers_county$year, na.rm = TRUE): #earliest year in data
                  max(farmers_county$year, na.rm = TRUE)) #latest year in data
#Expand law data into a panel (one row per state-year)
laws_panel <- laws_clean |>
  select(-year) |> #remove existing year column
  crossing(years) |> #creates all combinations of laws data with each year
  mutate(
    adoption_year = as.numeric(adoption_year), 
    year = as.numeric(year)
  ) |>
  mutate(
    #create an indicator variable of if the law is active in that year
    law_active = ifelse(year >= adoption_year, 1, 0),
    #number of years since the law was adopted
    years_since_adoption = ifelse(year >= adoption_year, year - adoption_year, 0),
    #indicator for whetehr the state ever adopts the law
    treated = ifelse(!is.na(adoption_year), 1, 0)
  )



#Merging the datasets

#Dataset 1 - Farmers markets
farmers_analysis <- farmers_county |>
  left_join(laws_panel, by = c("state", "year")) |>
  #keep only valid US states
  filter(!is.na(state), state != "") |>
  #drop missing key variables
  filter(
    !is.na(market_ops),
    !is.na(treated),
    !is.na(year)
  )

#Dataset 2 - Census value added
census_analysis <- census_clean |>
  left_join(laws_panel, by = c("state", "year")) |>
  filter(!is.na(state), state != "")


#Save our cleaned datasets
save(farmers_analysis, file = "farmers_law_data.RData")

save(census_analysis, file = "census_law_data.RData")





