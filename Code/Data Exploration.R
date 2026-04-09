
#install necessary packages
#install.packages(c("tidyverse), "scales")

library(tidyverse)
library(scales)
library(ggplot2)



#loading the datasets
load("census_law_data.RData") #cleaned county-level census data with law info
load("farmers_law_data.RData") #cleaned county-level farmers market data with law info

#ensuring key variables are numeric
# For farmers
farmers_analysis <- farmers_analysis %>%
  mutate(market_ops = as.numeric(market_ops))

# For census
census_analysis <- census_analysis %>%
  mutate(farms_value_added = as.numeric(farms_value_added))

#for this analysis I will just be looking at number of 
  #operations using the farmers market data and census data

#lets look at the data before changing anything
#histogram of farmers market distribution
ggplot(farmers_analysis, aes(x = market_ops)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
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
ggplot(census_analysis, aes(x = farms_value_added)) +
  geom_histogram(bins = 40, fill = "purple", color = "white") +
  labs(
    title = "Distribution of Farms with Value-Added Sales",
    x = "Number of Farms",
    y = "Count"
  ) +
  theme_minimal()

summary(census_analysis$farms_value_added)

quantile(census_analysis$farms_value_added, probs = c(0.5, 0.9, 0.95, 0.99), na.rm = TRUE)

#since none of the 99 percentiles look substantially larger than our
  #95 percentiles ones I will not be dealing with extreme values 
#both have long right tails so lets go ahead and log transform and look again

#farmers market data
ggplot(farmers_analysis, aes(x = log1p(market_ops))) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(
    title = "Log Distribution of Farmers Markets",
    x = "log(1 + markets)",
    y = "Count"
  ) +
  theme_minimal()

#census farm count data (adding 1 to deal with 0s)
ggplot(census_analysis, aes(x = log1p(farms_value_added))) +
  geom_histogram(bins = 40, fill = "purple", color = "white") +
  labs(
    title = "Log Distribution of Farms with Value-Added Sales",
    x = "log (1 + Number of Farms)",
    y = "Count"
  ) +
  theme_minimal()


#great those distributions look more appropriate
#moving on to visualization


#Figure 1: time trend (farmers markets)
fig1_data <- farmers_analysis %>%
  group_by(year) %>%
  summarize(
    avg_markets = mean(market_ops, na.rm = TRUE),
    .groups = "drop"
  )

fig1 <- ggplot(fig1_data, aes(x = year, y = avg_markets)) +
  geom_line(size = 1, color = "darkblue") +
  geom_point(color = "darkblue") +
  labs(
    title = "Figure 1: Average Farmers Markets per County Over Time",
    x = "Year",
    y = "Average Markets",
    caption = "Shows overall trend in farmers market prevalence."
  ) +
  theme_minimal()
print(fig1)


#time trend farms (census)
fig2_data <- census_analysis %>%
  group_by(year) %>%
  summarize(
    avg_farms = mean(farms_value_added, na.rm = TRUE),
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
            hjust = -0.15, size = 2.8, color = "darkgrey") +
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
    mean_farms = mean(farms_value_added, na.rm = TRUE),
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
            hjust = -0.15, size = 2.8, color = "darkgrey") +
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



