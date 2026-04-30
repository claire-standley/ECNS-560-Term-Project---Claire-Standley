#SET UP---------------------------------------------------------------
#install.packages(c("dplyr", "fixest", "modelsummary", "kabelExtra", "webshot2"))

#loading necessary packages
library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)
library(webshot2)

#LOADING THE DATA-----------------------------------------------------------------
#loading the cleaned datasets

#census data loading from github
census_temp <- tempfile(fileext = ".RData")
download.file("https://raw.githubusercontent.com/claire-standley/ECNS-560-Term-Project---Claire-Standley/main/Cleaned%20Data/census_law_data.RData",
              census_temp, mode = "wb")
load(census_temp)

#farmers market data loading from github
farmers_temp <- tempfile(fileext = ".RData")
download.file("https://raw.githubusercontent.com/claire-standley/ECNS-560-Term-Project---Claire-Standley/main/Cleaned%20Data/farmers_law_data.RData",
              farmers_temp, mode = "wb")
load(farmers_temp)


#FARMERS MARKET DATA PREP--------------------------------------------------------

#note market_ops counts new registrations only, not total active markets
  #this likely understates true market prevalence so keep in mind for interpretation

farmers_reg <- farmers_analysis |>
  mutate(
    market_ops = as.numeric(market_ops), #makes sure variable is numeric
    log_market = log1p(market_ops), #log transform outcome
    treated = as.integer(!is.na(adoption_year) & year >= adoption_year), #define treatment indicator
    county_state = paste(county, state, sep = "_"), #make unique county identifier
    year = as.integer(year) #make sure year is numeric
  )

#quick dataset diagnostics (more reader friendly)
writeLines(c(
  "--- Farmers Market Dataset ---",
  #paste number of observations
  paste("Observations:", nrow(farmers_reg)),
  #list of years included in dataset
  paste("Years covered:", paste(sort(unique(farmers_reg$year)), collapse = ", ")),
  #number of unique counties
  paste("Counties:", dplyr::n_distinct(farmers_reg$county_state))
))



#CENSUS DATA PREP-------------------------------------------------------------

census_reg <- census_analysis |>
  mutate(
    farms_operations = as.numeric(farms_operations), #make sure outcome is numeric
    log_farms = log1p(farms_operations), #log transformation
    treated = as.integer(!is.na(adoption_year) & year >= adoption_year), #treatment indicator
    county_state = paste(county, state, sep = "_"), #county level fixed effect identifier
    year = as.integer(year) #makse sure year is numeric
  )

writeLines(c(
  "--- Census Dataset ---",
  paste("Observations:", nrow(census_reg)),
  paste("Years covered:", paste(sort(unique(census_reg$year)), collapse = ", ")),
  paste("Counties:", dplyr::n_distinct(census_reg$county_state))
))


#STATE LEVEL CENSUS DATA SUMMARY---------------------------------------------------
#State-Level Summary Table (Census Data)

#showing mean value added farms per county by state, ranked descending, with law adoption status flagged.
#identifies which high productive states lack liability laws, motivating robustness checks

state_summary <- census_reg |>
  group_by(state) |> #aggregate all observations within each state
  summarize(
    mean_farms_per_county = round(mean(farms_operations, na.rm = TRUE), 1),
    #average number of value added farms per county (within state)
    n_counties = n_distinct(county_state), #number of unique counties observed in the state
    has_law = ifelse(any(!is.na(adoption_year)), "Yes", "No"), #indicator for EVER adopted
    law_year = ifelse(any(!is.na(adoption_year)), #identifies the FIRST year the law was adopted
                      as.character(min(adoption_year, na.rm = TRUE)),
                      "None"),
    .groups = "drop" #prevents grouped output from carrying forward
  ) |>
  arrange(desc(mean_farms_per_county))|> #sort states from highest to lowest ag activity
  mutate(rank = row_number()) |> #assign rank based on sorted order
  select(rank, state, mean_farms_per_county, n_counties, has_law, law_year) #keep only relevant columns

#print table(all states)
writeLines("--- State Summary: Mean Value-Added Farms per County ---")
print(state_summary, n = 50) #n = 50 makes sure all stats are printed

#identify the high production non adoptors (top 10, no law)
high_prod_no_law <- state_summary |>
  filter(rank <= 10, #top 10 states by farm activity
         has_law == "No") |> # restrict to states without the law
  pull(state) #extract only the state names as a vector

writeLines(c(
  "",
  "High-production states (top 10) WITHOUT agritourism liability laws:",
  #collapse = ", " turns vector into a readable string
  paste(high_prod_no_law, collapse = ", "),
  ""
))


#ROBUSTNESS EXCLUSION SETS----------------------------------------------------------

#Important things to note:
  #the model regression will use TWFE to control for county and year in both datasets
  #standard errors are clustered at the state level because treatment is assigned at the state level, not county
  #this is a descriptive regression used as a first step toward causal inference
  #beta should not be interpreted as a causal effect. 

#upon doing the state summary it is apparent there are several higher producing states in the control
#group that are atypical (dense coastal states with established food cultures and high consumer incomes)
#that likely drive DTC activity regardless of legal environments. This is likely understating the true 
#beta when running just a regular regression. As a result several robustness checks are conduced as follows

#Specification 1: Exclude Connecticut only (largest single concern)
#Specification 2: Exclude Connecticut, California, and Massachusetts (only those non adopters in the top 5)
#Specification 3: Exclude Connecticut, California, Massachusetts, and New Hampshire (all non adopters in top 10 - census)
#Specification 4: Exclude Connecticut, California, Massachusetts, and Arizona (top 4 non adopters - farmers market data)

#IMPORTANT: exclusions are conduced only as a robustness check and not for the purpose of cherry picking states
  #important understanding of the pre period needs to be understood as non adopters are likely not a good proxy

#define exclusion sets
excl_a <- c("CONNECTICUT")
excl_b <- c("CONNECTICUT", "CALIFORNIA", "MASSACHUSETTS")
excl_c <- c("CONNECTICUT", "CALIFORNIA", "MASSACHUSETTS", "NEW HAMPSHIRE")
excl_d <- c("CONNECTICUT", "CALIFORNIA", "MASSACHUSETTS", "ARIZONA")

#create excluded data sets
farmers_excl_a <- farmers_reg |> filter(!state %in% excl_a)
farmers_excl_b <- farmers_reg |> filter(!state %in% excl_b)
farmers_excl_d <- farmers_reg |> filter(!state %in% excl_d)

census_excl_a <- census_reg |> filter(!state %in% excl_a)
census_excl_b <- census_reg |> filter(!state %in% excl_b)
census_excl_c <- census_reg |> filter(!state %in% excl_c)

#sample size check
sample_sizes <- tibble(
  Sample = c(
    "Full sample",
    "Ex. California",
    "Ex. CA + CT + MA",
    "Ex. CA + CT + MA + HI"
  ),
  Observations = c(
    nrow(census_reg),
    nrow(census_excl_a),
    nrow(census_excl_b),
    nrow(census_excl_c)
  )
)

sample_sizes

#REGRESSION MODELS (COUNTY)-------------------------------------------------------------------

#Main Model - full sample
#Model 1: New farmers market registrations (log) ~ law adoption
model1 <- feols(
  log_market ~ treated | county_state + year,
  data = farmers_reg,
  cluster = ~state #cluster SE at the state level
)


#Model 2: Value added farms (log) census ~ law adoption
model2 <- feols(
  log_farms ~ treated | county_state + year,
  data = census_reg,
  cluster = ~state
)


summary(model1)
summary(model2)


#Robustness A - exclude Connecticut
#Model 3: New farmers market registrations (log) ~ law adoption
model3 <- feols(
  log_market ~ treated | county_state + year,
  data = farmers_excl_a,
  cluster = ~state
)


#Model 4: Value added farms (log) census ~ law adoption
model4 <- feols(
  log_farms ~ treated | county_state + year,
  data = census_excl_a,
  cluster = ~state
)


summary(model3)
summary(model4)


#Robustness B - exclude Connecticut, California, and Massachusetts
#Model 5: New farmers market registrations (log) ~ law adoption
model5 <- feols(
  log_market ~ treated | county_state + year,
  data = farmers_excl_b,
  cluster = ~state
)


#Model 6: Value added farms (log) census ~ law adoption
model6 <- feols(
  log_farms ~ treated | county_state + year,
  data = census_excl_b,
  cluster = ~state
)


summary(model5)
summary(model6)

#Robustness C - exclude Connecticut, California, Massachusetts, and New Hampshire (census)
            #OR exlude Connecticut, California, Massachusetts, and Arizona (farmers market)
#Model 7: New farmers market registrations (log) ~ law adoption
model7 <- feols(
  log_market ~ treated | county_state + year,
  data = farmers_excl_d,
  cluster = ~state
)


#Model 8: Value added farms (log) census ~ law adoption
model8 <- feols(
  log_farms ~ treated | county_state + year,
  data = census_excl_c,
  cluster = ~state
)


summary(model7)
summary(model8)



#DIAGNOSTIC SUMMARY: HOW MUCH DID BETA SHIFT?


# Census model
census_results <- tibble(
  Sample = c(
    "Full sample",
    "Ex. Connecticut",
    "Ex. CT + CA + MA",
    "Ex. CT + CA + MA + NH"
  ),
  Beta = c(
    coef(model2)["treated"], #full sample
    coef(model4)["treated"], #excl CT
    coef(model6)["treated"], #excl CT + CA + MA
    coef(model8)["treated"]  #excl CT + CA + MA + NH
  )
) |>
  mutate(Change = Beta - Beta[1], #difference relative to full sample estimate
         Model = "Census (Value-Added Farms)")

# Farmers market model
fm_results <- tibble(
  Sample = c(
    "Full sample",
    "Ex. Connecticut",
    "Ex. CT + CA + MA",
    "Ex. CT + CA + MA + AZ"
  ),
  Beta = c(
    coef(model1)["treated"], #full sample
    coef(model3)["treated"], #excl CT
    coef(model5)["treated"], #excl CT + CA + MA
    coef(model7)["treated"]  #excl CT + CA + MA + AZ
  )
) |>
  mutate(Change = Beta - Beta[1],
         Model = "Farmers Market")

# Combine results
robustness_table <- bind_rows(census_results, fm_results)

robustness_table


#REGRESSION TABLES--------------------------------------------

#rename "treated" to a more interpretable label in tables
coef_labels <- c("treated" = "Agritourism Liability Law Adopted (=1)")

#Table 1: Main results (models 1 and 2 only)
modelsummary(
  list(
    "(1) Log(New Market Registrations)" = model1,
    "(2) Log(Value-Added Farms)" = model2
  ),
  coef_map = coef_labels, #replaces variable names with readable labels
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01), #significance stars (standard convention)
  gof_map = c("nobs", "r.squared"), #only display key fit stats
  title = "Table 1. Agritourism Liability Law Adoption and Local Food Market Activity (Full Sample)",
  notes = c(
    "Standard errors clustered at the state level in parentheses.",
    "All models include county and year fixed effects. Outcomes are log(1 + count).",
    "Model 1: USDA Local Food Directory, 2022-present (annual new registrations only).",
    "Model 2: USDA Census of Agriculture, 1997-2022 (5-year intervals, total farms)."
  ),
  output = "figures/table1_main.png"
)


#Table 2: Robustness checks (census model across all exclusions)
modelsummary(
  list(
    "(1) Full Sample" = model2,
    "(2) Ex. Connecticut" = model4,
    "(3) Ex. CT + CA + MA" = model6,
    "(4) Ex. CT + CA + MA + NH" = model8
  ),
  coef_map = coef_labels,
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  title = "Table 2. Robustness Check: Excluding High-Production Non-Adopting States (Census Model)",
  notes = c(
    "Dependent variable: log(1 + value-added farms per county). USDA Census of Agriculture.",
    "Standard errors clustered at the state level in parentheses.",
    "All models include county and year fixed effects.",
    "Excluded states are high-production non-adopters identified in Figure 5.",
    "Exclusions are sensitive checks; the full-sample estimate (col. 1) is the main result."
  ),
  output = "figures/table2_robustness_census.png"
)


#Table 4: Robustness checks (farmers market model across all exclusions)
modelsummary(
  list(
    "(1) Full Sample" = model1,
    "(2) Ex. Connecticut" = model3,
    "(3) Ex. CT + CA + MA" = model5,
    "(4) Ex. CT + CA + MA + AZ" = model7
  ),
  coef_map = coef_labels,
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  title = "Table 3. Robustness Check: Excluding High-Production Non-Adopting States (Farmers Market Model)",
  notes = c(
    "Dependent variable: log(1 + new farmers market registrations per county). USDA Local Food Directory.",
    "Standard errors clustered at the state level in parentheses.",
    "All models include county and year fixed effects.",
    "Excluded states are high-production non-adopters identified in Figure 4.",
    "Exclusions are sensitive checks; the full-sample estimate (col. 1) is the main result."
  ),
  output = "figures/table3_robustness_farmers.png"
)


#STATE LEVEL AGGREGATION---------------------------------------

#since treatment is assigned at the state level aggregating to state-year
#removes within state county variation that is not informative for 
#identifying the effect. The mean is taken across counties within each 
#state year cell. 

#Farmers market: aggregate to state year
farmers_state <- farmers_reg |>
  group_by(state, year) |> #group data by state and year
  summarize(
    mean_log_market = mean(log_market, na.rm = TRUE),
    total_markets = sum(market_ops, na.rm = TRUE),
    n_counties = n(),
    treated = first(treated), #same for all counties in state
    adoption_year = first(adoption_year), 
    .groups = "drop"
  ) |>
  mutate(log_total_market = log1p(total_markets))

#Census: aggregate to state year
census_state <- census_reg |>
  group_by(state, year) |>
  summarize(
    mean_log_farms = mean(log_farms, na.rm = TRUE),
    total_farms = sum(farms_operations, na.rm = TRUE),
    n_counties = n(),
    treated = first(treated),
    adoption_year = first(adoption_year),
    .groups = "drop"
  ) |>
  mutate(log_total_farms = log1p(total_farms))

#create a simple summary of the resulting data sets
panel_summary <- tibble(
  Dataset = c("Farmers market", "Census"),
  #number of observations
  Observations = c(
    nrow(farmers_state),
    nrow(census_state)
  ),
  #number of unique states in each data set
  States = c(
    n_distinct(farmers_state$state),
    n_distinct(census_state$state)
  ),
  #list of years covered in each data set
  Years = c(
    paste(sort(unique(farmers_state$year)), collapse = ", "),
    paste(sort(unique(census_state$year)), collapse = ", ")
  )
)
#display summary table
panel_summary

#REGRESSIONS MODELS (STATE)---------------------------------------

#note: since state is the unit of observation standard errors are no longer clusters
  #HC robust SE will be used

#Farmers Market Models

#Model 1: farmers market - mean of county logs
model_s1 <- feols(mean_log_market ~ treated | state + year,
                  data = farmers_state)

#Model 2: farmers market - log of state total
model_s2 <- feols(log_total_market ~ treated | state + year,
                  data = farmers_state)
#Census Models

#Model 3: value added farms - mean of county logs
model_s3 <- feols(mean_log_farms ~ treated | state + year,
                  data = census_state)

#Model 4: value added farms - log of state total
model_s4 <- feols(log_total_farms ~ treated | state + year,
                  data = census_state)

#display regression results
summary(model_s1)
summary(model_s2)
summary(model_s3)
summary(model_s4)


#robustness - high production non adopters state level
farmers_state_rob <- farmers_state |> filter(!state %in% excl_b)
census_state_rob <- census_state |> filter(!state %in% excl_b)

#re run models on restricted sample

#Model 5: farmers market (mean logs), excluding selected states
model_s5 <- feols(mean_log_market ~ treated | state + year,
                  data = farmers_state_rob)

#Model 6: farms (log totals), excluding selected states
model_s6 <- feols(log_total_farms ~ treated | state + year,
                  data = census_state_rob)

#combine all models into a formatted table
modelsummary(
  list(
    "(1) Mean Log Markets"     = model_s1,
    "(2) Log Total Markets"    = model_s2,
    "(3) Mean Log Farms"       = model_s3,
    "(4) Log Total Farms"      = model_s4,
    "(5) Mean Log Markets\nEx. CA/CT/MA" = model_s5,
    "(6) Log Total Farms\nEx. CA/CT/MA" = model_s6
  ),
  #rename coefficients for readability
  coef_map = coef_labels,
  stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map  = c("nobs", "r.squared"),
  title    = "Table 4. Agritourism Liability Law Adoption and Local Food Market Activity (State-Year Panel)",
  notes    = c(
    "Unit of observation is the state-year.",
    "All models include state and year fixed effects.",
    "Robust standard errors in parentheses (HC1).",
    "Models 1-2: USDA Local Food Directory, 2022-present.",
    "Models 3-4: USDA Census of Agriculture, 1997-2022.",
    "Models 5-6: California, Connecticut, and Massachusetts excluded."
  ), 
  output = "figures/table4_aggregate.png"
)



