#GETTING PACKAGES--------------------------------------------------------------------

#install necessary packages
#install.packages(c("tidyverse, "scales", "ggplot2", "kableExtra", "ggrepel"))

library(tidyverse)
library(scales)
library(ggplot2)
library(kableExtra)
library(ggrepel) #for non overlapping scatter labels


#LOADING THE DATA-------------------------------------------------------------------
#loading the cleaned datasets from Github

#census data loading from github
census_temp <- tempfile(fileext = ".RData") #create temporary file path
download.file("https://raw.githubusercontent.com/claire-standley/ECNS-560-Term-Project---Claire-Standley/main/Cleaned%20Data/census_law_data.RData",
              census_temp, mode = "wb") #ensure proper download of binary files
load(census_temp) #loads objects into the current environment

#farmers market data loading from github
farmers_temp <- tempfile(fileext = ".RData")
download.file("https://raw.githubusercontent.com/claire-standley/ECNS-560-Term-Project---Claire-Standley/main/Cleaned%20Data/farmers_law_data.RData",
              farmers_temp, mode = "wb")
load(farmers_temp)


#ensuring key variables are numeric
# For farmers
farmers_analysis <- farmers_analysis |>
  mutate(
    market_ops = as.numeric(market_ops), #ensure number of market operations is numeric
    log_market = log1p(market_ops), #log transformation (log(1 + x) to handle zeros)
    county_state = paste(county, state, sep = "_") #create unique county state ID 
  )

# For census
census_analysis <- census_analysis |>
  mutate(
    farms_value_added = as.numeric(farms_operations), #ensures value added farms variable is numeric
    log_farms = log1p(farms_operations), #log transformation of outcome variable
    county_state = paste(county, state, sep = "_") 
  )

#for this analysis number of operations from the farmers market data and census data will be used

#HISTOGRAM DISTRIBUTIONS---------------------------------------------------------
#lets look at the data before changing anything
#histogram of farmers market distribution
ggplot(farmers_analysis, aes(x = market_ops)) +
  geom_histogram(bins = 40, fill = "darkblue", color = "white") +
  labs(
    title = "Distribution of Farmers Markets per County",
    x = "Number of Farmers Markets",
    y = "Count"
  )+
  theme_minimal()
#summary stats
summary(farmers_analysis$market_ops)
#quantiles (to look for extreme values)
quantile(farmers_analysis$market_ops, 
         probs = c(0.5, 0.9, 0.95, 0.99), na.rm = TRUE)


#Number of Farms (census)
#histogram
ggplot(census_analysis, aes(x = farms_operations)) +
  geom_histogram(bins = 40, fill = "darkgreen", color = "white") +
  labs(
    title = "Distribution of Farms with Value-Added Sales",
    x = "Number of Farms",
    y = "Count"
  ) +
  theme_minimal()

summary(census_analysis$farms_operations)

quantile(census_analysis$farms_operations, probs = c(0.5, 0.9, 0.95, 0.99), na.rm = TRUE)

#since none of the 99 percentiles look substantially larger than our
  #95 percentiles ones I will not be dealing with extreme values 
#strong right skew on both variables skewness may violate linear model assumptions
#both have long right tails so lets go ahead and log transform and look again
  #this log transofrmation is an effort to reduce right skew, handle zeros, and change interpretation for later

#farmers market data
ggplot(farmers_analysis, aes(x = log1p(market_ops))) +
  geom_histogram(bins = 40, fill = "darkblue", color = "white") +
  labs(
    title = "Log Distribution of Farmers Markets",
    x = "log(1 + markets)",
    y = "Count"
  ) +
  theme_minimal()

#census farm count data (adding 1 to deal with 0s)
ggplot(census_analysis, aes(x = log1p(farms_operations))) +
  geom_histogram(bins = 40, fill = "darkgreen", color = "white") +
  labs(
    title = "Log Distribution of Farms with Value-Added Sales",
    x = "log (1 + Number of Farms)",
    y = "Count"
  ) +
  theme_minimal()


#DATA EXPLORING----------------------------------------------------------------------
#great those distributions look more appropriate
#moving on to further exploration of the data

#add ever treated group label to both datasets
#a state is "treated" if it ever adopts the law (non missing adoption year)
farmers_analysis <- farmers_analysis |>
  group_by(state) |> #group by state to evaluate treatment at the STATE level
  mutate(ever_treated = ifelse(!is.na(adoption_year), "Treated", "Never Treated")) |>
  ungroup() #ungroup to avoid accidental grouped behavior later

#first make sure one observation per state county year
census_analysis <- census_analysis |>
  group_by(state, county, year) |> #define the level at which observations should be unique
  arrange(adoption_year) |> #sort rows so early adoption comes first
  slice(1) |> #keep ONLY the first row per group (removes duplicates)
  ungroup() |>
  group_by(state) |>
  #state is treated if it ever has a non missing adoption year
  mutate(ever_treated = ifelse(any(!is.na(adoption_year)), "Treated", "Never Treated")) |>
  ungroup()

#county how many states adopt in each year and cumulative adoption
adoption_by_year <- census_analysis |>
  distinct(state, adoption_year) |> #keep one row per state
  filter(!is.na(adoption_year)) |> #drop states that never adopt
  mutate(adoption_year = as.integer(adoption_year)) |>
  count(adoption_year) |> #count how many states adopt in each year
  arrange(adoption_year) |> #make sure its chronological
  mutate(cumulative = cumsum(n)) #running total of states that have adpoted by each year
print(adoption_by_year) #table output

#visualization - staggered policy rollout
fig_rollout <- ggplot(adoption_by_year, aes(x = adoption_year)) +
  geom_col(aes(y = n), fill = "purple", alpha = 0.7, width = 0.6) +
  #bars are the number of NEW states adopting in each year
  geom_line(aes(y = cumulative / 5), color = "darkred", linewidth = 1) +
  geom_point(aes(y = cumulative / 5), color = "darkred", size = 2) +
  #the line is the cumulative adoption
  scale_y_continuous(
    name = "States Adoption in That Year",
    sec.axis = sec_axis(~ . * 5, name = "Cumulative States with Law")
  ) + #adds a dual scale
  scale_x_continuous(breaks = seq(2000, 2025, by = 2)) +
  labs(
    title = "Figure A1. Agritourism Liability Law Adoption Over Time",
    subtitle = "Bars = new adoptions per year | Red line = cumulative states with law",
    x = "Year",
    caption = "Source: National Agricultural Law Center"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "darkred"),
    axis.text.y.right = element_text(color = "darkred")
  )
print(fig_rollout)

#unconditional means: treated vs never treated
#census outcomes
census_comparison <- census_analysis |>
  group_by(ever_treated) |> #seperate treated vs never treated groups
  summarize(
    n_states = n_distinct(state), #number of unique states
    n_obs = n(), #total observations
    mean_farms = round(mean(farms_operations, na.rm = TRUE), 1),
    median_farms = round(median(farms_operations, na.rm = TRUE), 1),
    mean_log_farms = round(mean(log_farms, na.rm = TRUE), 3),
    .groups = "drop"
  )

#farmers market outcomes
farmers_comparison <- farmers_analysis |>
  group_by(ever_treated) |>
  summarize(
    n_states = n_distinct(state),
    n_obs = n(),
    mean_markets = round(mean(market_ops, na.rm = TRUE), 1),
    median_markets = round(median(market_ops, na.rm = TRUE), 1),
    mean_log_markets = round(mean(log_market, na.rm = TRUE), 3),
    .groups = "drop" #prevents grouped output from carrying forward
  )

#store together for easy viewing/export
comparison_outputs <- list(
  "Census: Treated vs. Never-Treated" = census_comparison,
  "Farmers Market: Treated vs. Never-Treated" = farmers_comparison
)

comparison_outputs

#observation counts by treatment status and year
census_counts <- census_analysis |>
  count(year, ever_treated) |> #count observation by year and treatment status
  pivot_wider(names_from = ever_treated, values_from = n, values_fill = 0) |>
  #converts from long to wide format for easier comparison
  arrange(year) |>
  mutate(Source = "Census")

farmers_counts <- farmers_analysis |>
  count(year, ever_treated) |>
  pivot_wider(names_from = ever_treated, values_from = n, values_fill = 0) |>
  arrange(year) |>
  mutate(Source = "Farmers Market")

#combine datasets for side by side comparison
combined_counts <- bind_rows(census_counts, farmers_counts)

combined_counts

#census trends
#time trend - treated vs never treated
trend_census <- census_analysis |>
  group_by(year, ever_treated) |>
  summarize(
    mean_log = mean(log_farms, na.rm = TRUE),
    se_log = sd(log_farms, na.rm = TRUE) / sqrt(n()), #standard error = sd/sqrt(n)
    .groups = "drop"
  )

fig_trend_census <- ggplot(trend_census,
                           aes(x = year, y = mean_log,
                               color = ever_treated,
                               linetype = ever_treated)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Treated" = "orange",
                                "Never Treated" = "pink")) +
  scale_fill_manual(values = c("Treated" = "orange",
                               "Never Treated" = "pink")) +
  scale_x_continuous(breaks = c(1997, 2002, 2007, 2012, 2017, 2022)) +
  labs(
    title = "Figure A2. Value-Added Farms Over Time: Treated vs. Never-Treated States",
    subtitle = "Mean log(1 + Farms per County) by treatment group.",
    x = "Year",
    y = "Mean log(1 + Farms per County)",
    color = NULL, linetype = NULL, fill = NULL,
    caption = "Treated = ever adopted agritourism liability law. USDA Census of Agriculture."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(fig_trend_census)

#farmers market trends
#time trend: treated vs never treated (farmers market)
trend_farmers <- farmers_analysis |>
  group_by(year, ever_treated) |>
  summarize(
    mean_log = mean(log_market, na.rm = TRUE),
    se_log = sd(log_market, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

fig_trend_farmers <- ggplot(trend_farmers,
                           aes(x = year, y = mean_log,
                               color = ever_treated,
                               linetype = ever_treated)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Treated" = "darkorange",
                                "Never Treated" = "hotpink")) +
  scale_fill_manual(values = c("Treated" = "darkorange",
                               "Never Treated" = "hotpink")) +
  scale_x_continuous(breaks = sort(unique(farmers_analysis$year))) +
  labs(
    title = "Figure A3. Farmers Market Registrations Over Time: Treated vs. Never-Treated States",
    subtitle = "Mean log(1 + New Registrations per County) by treatment group.",
    x = "Year",
    y = "Mean log(1 + Registrations per County)",
    color = NULL, linetype = NULL, fill = NULL,
    caption = "Treated = ever adopted agritourism liability law. USDA Local Food Directory."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(fig_trend_farmers)

#Singleton diagnosis - farmers market
county_year_counts <- farmers_analysis |>
  count(county_state, name = "n_years") #count how many years each county appears in the dataset

singleton_n <- sum(county_year_counts$n_years == 1) #number of counties observed only once
singleton_share <- mean(county_year_counts$n_years == 1) #fraction of counties that are singletons

singleton_summary <- tibble(
  Dataset = "Farmers Market",
  Singleton_Count = singleton_n,
  Total_Counties = nrow(county_year_counts),
  Share_Percent = round(singleton_share * 100, 0),
  Note = "Singletons dropped as fixed-effect singletons in TWFE regression"
)

singleton_summary

fig_singleton <- ggplot(county_year_counts, aes(x = n_years)) +
  geom_bar(fill = "yellow", alpha = 0.8) +
  scale_x_continuous(breaks = 1:max(county_year_counts$n_years)) +
  labs(
    title = "Figure A4. Years Each County Appears in the Farmers Market Dataset",
    subtitle = sprintf(
      "%.0f%% of counties appear in only one year and are dropped as singletons in the regression",
      singleton_share * 100),
    x = "Number of Years County Appears",
    y = "Number of Counties",
    caption = "USDA Local Food Directory, 2019-2026."
  ) +
  theme_minimal()

print(fig_singleton)


#cross dataset consistency: scatter in 2022
#aggregate farmers data to state level
farmers_2022 <- farmers_analysis |>
  filter(year == 2022) |>
  group_by(state) |>
  summarize(log_tot_markets = log1p(sum(market_ops, na.rm = TRUE)), 
            #total markets per state then log transform
            .groups = "drop")

#aggregate census data to state level
census_2022 <- census_analysis |>
  filter(year == 2022) |>
  group_by(state) |>
  summarize(log_mean_farms = log1p(mean(farms_operations, na.rm = TRUE)),
            #mean farms per county then log transform
            ever_treated   = first(ever_treated),
            .groups = "drop")

#merge datasets (only states appearing in both are kept)
cross_2022 <- inner_join(farmers_2022, census_2022, by = "state") |>
  mutate(state_fmt = tools::toTitleCase(tolower(state)))

#correlation between datasets
cor_val <- cor(cross_2022$log_tot_markets, cross_2022$log_mean_farms,
               use = "complete.obs")
cat(sprintf("\nCross-dataset correlation in 2022 (state level): r = %.3f\n", cor_val))

fig_cross <- ggplot(cross_2022,
                    aes(x = log_mean_farms, y = log_tot_markets,
                        color = ever_treated)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8, linetype = "dashed") +
  geom_text_repel(aes(label = state_fmt), size = 2.6, max.overlaps = 15) +
  scale_color_manual(values = c("Treated"       = "darkorange",
                                "Never Treated" = "hotpink")) +
  annotate("text", x = -Inf, y = Inf,
           label  = sprintf("r = %.2f", cor_val),
           hjust  = -0.3, vjust = 1.5, size = 4, fontface = "italic") +
  labs(
    title    = "Figure A5. Farmers Markets vs. Value-Added Farms by State (2022 Only)",
    subtitle = "State-level totals in the only year shared by both datasets. Dashed line = OLS fit.",
    x        = "log(Mean Value-Added Farms per County) — Census",
    y        = "log(Total New Market Registrations) — Local Food Directory",
    color    = NULL,
    caption  = "2022 is the single overlapping year between the two datasets."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(fig_cross)

#save all new figures
ggsave("figures/figA1_law_rollout.png",      fig_rollout,       width = 9, height = 5, dpi = 300)
ggsave("figures/figA2_trend_census.png",     fig_trend_census,  width = 9, height = 5, dpi = 300)
ggsave("figures/figA3_trend_farmers.png",    fig_trend_farmers, width = 9, height = 5, dpi = 300)
ggsave("figures/figA4_singleton_coverage.png", fig_singleton,   width = 8, height = 5, dpi = 300)
ggsave("figures/figA5_cross_dataset_2022.png", fig_cross,       width = 9, height = 6, dpi = 300)


#FINAL FIGURES------------------------------------------------------------------------
#now that the data is better understood lets move to visulization

#Figure 1: time trend (farmers markets)
fig1_data <- farmers_analysis |>
  group_by(year) |>
  summarize(
    avg_markets = mean(market_ops, na.rm = TRUE),
    .groups = "drop"
  )

fig1 <- ggplot(fig1_data, aes(x = year, y = avg_markets)) +
  geom_line(size = 1, color = "darkblue") +
  geom_point(color = "darkblue") +
  labs(
    title = "Figure 1: Average New Registration of Farmers Markets per County Over Time",
    x = "Year",
    y = "Average Markets",
    caption = "Shows overall trend in farmers market prevalence."
  ) +
  theme_minimal()
print(fig1)


#time trend farms (census)
fig2_data <- census_analysis |>
  group_by(year) |>
  summarize(
    avg_farms = mean(farms_operations, na.rm = TRUE),
    .groups = "drop"
  )

fig2 <- ggplot(fig2_data, aes(x = year, y = avg_farms)) +
  geom_line(size = 1, color = "darkgreen") +
  geom_point(color = "darkgreen") +
  labs(
    title = "Figure 2: Average Number of Farms with Value-Added Sales Over Time",
    x = "Year",
    y = "Average Number of Farms",
    caption = "Shows trends in value-added agricultural participation."
  ) +
  theme_minimal()
print(fig2)


#Figure 3: market operations by law complexity

farmers <- farmers_analysis |>
  mutate(
    log_market = log1p(market_ops),
    #year as factor for plotting
    year_fac = factor(year),
    #law complexity groups
    law_group = case_when(
      how_many_codes_laws <= 2 ~ "1-2 codes",
      how_many_codes_laws <= 5  ~ "3–5 codes",
      how_many_codes_laws <= 9  ~ "6–9 codes",
      TRUE                      ~ "10+ codes"
    ),
  law_group = factor(law_group,
                   levels = c("1–2 codes","3–5 codes",
                              "6–9 codes","10+ codes"))
)

law_summary <- farmers |>
  group_by(law_group) |>
  summarize(
    n = n(), #count of counties
    mean_market = mean(market_ops, na.rm = TRUE), #mean market operations
    .gropus = "drop" #remove grouping
  )

fig3 <- ggplot(law_summary, aes( x = law_group, y = mean_market)) +
  geom_col(fill = "purple", alpha = 0.7, width = 0.6) +
  geom_text(aes(label = paste0("n =", n)),
            vjust = 0, size = 3.2, color = "black") +
  labs(
    title = "Figure 3. Mean Market Operations by Law Complextiy",
    subtitle = "Average number of direct-market operations per county, by number of statutory codes",
    x = "Number of Statutory Codes in State Law",
    y = "Mean Market Operations per County"
  ) +
  theme_minimal()
print(fig3)


#Figure 4: state level variation in mean market operations (farmers market)
state_avg <- farmers |>
  group_by(state) |>
  summarize(
    mean_market = mean(market_ops, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) |>
  arrange(mean_market) |>
  mutate(
    state_fmt = tools::toTitleCase(tolower(state)), #proper case for labels
    state_fmt = fct_inorder(state_fmt) #keep order in plot
  )

fig4 <- ggplot(state_avg, aes(x = mean_market, y = state_fmt, fill = mean_market)) +
  geom_col(alpha = 0.85) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  geom_text(aes(label = round(mean_market, 1)),
            hjust = -0.15, size = 2.8, color = "black") +
  labs(
    title = "Figure 4. Mean Direct-Market Operations by State",
    subtitle = "All treated states | USDA Local Food Directories 2019-2026",
    x = "Mean Number of Market Operations per County",
    y = NULL,
    caption = "Darker blue = higher mean"
  ) +
  theme_minimal()
print(fig4)


#Figure 5. state level variation in mean farms (census data)
state_avg <- census_analysis |>
  group_by(state) |>
  summarize(
    mean_farms = mean(farms_operations, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) |>
  arrange(mean_farms) |>
  mutate(
    state_fmt = tools::toTitleCase(tolower(state)),
    state_fmt = fct_inorder(state_fmt)
  )

fig5 <- ggplot(state_avg, aes(x = mean_farms, y = state_fmt, fill = mean_farms)) +
  geom_col(alpha = 0.85) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  geom_text(aes(label = round(mean_farms, 1)),
            hjust = -0.15, size = 2.8, color = "black") +
  labs(
    title = "Figure 5. Average Number of Farms with Value-Added Sales by State",
    subtitle = "USDA Census Data (County-Level Aggregation)",
    x = "Mean Number of Farms per County",
    y = NULL,
    caption = "Darker green indicates higher averages"
  ) +
  theme_minimal()
print(fig5)


#Saving figures 
# Create a folder for figures
dir.create("figures", showWarnings = FALSE)

# Save each figure
ggsave("figures/fig1.png", plot = fig1, width = 8, height = 5, dpi = 300)
ggsave("figures/fig2.png", plot = fig2, width = 8, height = 5, dpi = 300)
ggsave("figures/fig3.png", plot = fig3, width = 8, height = 5, dpi = 300)
ggsave("figures/fig4.png", plot = fig4, width = 8, height = 5, dpi = 300)
ggsave("figures/fig5.png", plot = fig5, width = 8, height = 5, dpi = 300)



