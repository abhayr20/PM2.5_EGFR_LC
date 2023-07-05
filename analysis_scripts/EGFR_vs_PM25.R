library(tidyverse)
library(ggplot2)
library(ggrepel)

setwd("~/Desktop/AIIMS/letter_to_editor")

western_countries <- c('AUS', 'AUT', 'CAN', 'CZE', 'DEU',
                       'ESP', 'FIN', 'FRA', 'GBR', 'GRC',
                       'ITA', 'LTU', 'NLD', 'NOR', 'POL',
                       'PRT', 'RUS', 'SVK', 'SWE', 'USA')

## Get input data
globocan_lc_incidence <- read.csv("data/Global_estimated_lung_cancer_incidence_rates_both_sexes.csv")
EGFR_lc <- read.csv("data/EGFR_positive_estimated_incidence.csv")
df_pol <- read.csv("data/PM2.5-air-pollution.csv")
df_clean_fuel <- read.csv("data/Clean_fuel.csv")

# Prepare Lung cancer incidence rates for 36 countries
df_EGFR <- inner_join(globocan_lc_incidence, EGFR_lc, by = "ISO_code")
df_EGFR <- subset(df_EGFR, select = -Country_name)
df_EGFR$EGFR_incidence_rate <- (df_EGFR$LC_ASR_both_sexes * df_EGFR$mEGFR_freq_overall)/100

# Filter df_pol to only year 2017
df_pol <- filter(df_pol, year == 2017)
df_clean_fuel <- filter(df_clean_fuel, Year == 2017)

# Get final dataframes for modeling
df_EGFR_pol <- merge(df_EGFR, df_pol, by = "ISO_code") %>%
  select(ISO_code, Country, PM2.5, EGFR_incidence_rate)

west_df_EGFR_pol <- df_EGFR_pol[df_EGFR_pol$ISO_code %in% western_countries,]

## Scatterplot PM 2.5 levels vs EGFR+ NSCLC incidence rate
options(ggrepel.max.overlaps = Inf)

# Create the scatter plot using ggplot2 and ggrepel
p <- ggplot(df_EGFR_pol, aes(x = PM2.5, y = EGFR_incidence_rate, label = Country)) +
  geom_point(size = 3, color = ifelse(df_EGFR_pol$ISO_code %in% western_countries, "red", "blue")) +
  geom_text_repel(size = 6, nudge_x = 0.25, nudge_y = 0.07) +
  geom_smooth(data = df_EGFR_pol, method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  geom_smooth(data = west_df_EGFR_pol, method = "lm", se = FALSE, color = "red", formula = y ~ x + I(x > 5)) +
  labs(title = "Effect of outdoor pollution on EGFR driven LC",
       x = "PM2.5 levels (µg/m³) in 2017",
       y = "Estimated EGFR driven LC incidence (per 100,000)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.caption = element_text(hjust = 1, size = 16))

# Calculate the R value for df_EGFR_pol
outdoor_model <- lm(EGFR_incidence_rate ~ PM2.5, data = df_EGFR_pol)
r_squared <- summary(outdoor_model)$r.squared
p_value <- summary(outdoor_model)$coef[2, 4]  # Extract the p-value

# Calculate the R value for west_df_EGFR_pol
outdoor_model_west <- lm(EGFR_incidence_rate ~ PM2.5, data = west_df_EGFR_pol)
r_squared_west <- summary(outdoor_model_west)$r.squared
p_value_west <- summary(outdoor_model_west)$coef[2, 4]  # Extract the p-value

# Add the R-squared and p-value to the plot
scatter_EGFR_PM2.5 <- p +
  annotate("text", x = max(df_EGFR_pol$PM2.5), y = max(df_EGFR_pol$EGFR_incidence_rate),
           label = paste0("All Countries\nR² = ", round(r_squared, 3), "\np-value = ", round(p_value, 3)),
           hjust = 1, vjust = 2.5, size = 4, fontface = "bold", color = "blue") +
  annotate("text", x = max(df_EGFR_pol$PM2.5), y = max(df_EGFR_pol$EGFR_incidence_rate),
           label = paste0("Western Countries only\nR² = ", round(r_squared_west, 3), "\np-value = ", round(p_value_west, 3)),
           hjust = 1, vjust = 1, size = 4, fontface = "bold", color = "red")

# Display the scatter plot with R-squared, p-values, and updated settings
scatter_EGFR_PM2.5



'''
Instructions:

I have 2 dataframes: df_EGFR_pol and west_df_EGFR_pol. 
df_EGFR_pol has 4 columns which are: "ISO_code", "Country", "PM2.5" and "EGFR_incidence_rate". It also has 36 rows, each row containing data for 1 country
west_df_EGFR_pol is a subset of df_EGFR_pol having the 4 same columns but only 20 rows (20 unique countries).
I want to do a linear regression in R with PM2.5 on x axis and EGFR_incidence_rate on the y axis along with proper statistical testing.
I want to fit 2 regression lines first using data from all countries df_EGFR_pol and second using data from west_df_EGFR_pol.
I want the dot for the countries in west_df_EGFR_pol to be coloured red, the rest of the countries should be coloured blue. This should be accompanied by labels outside the plot area in the bottom right corner.
I should calculate the R-sqaured and p-values for both the models again, and report both outside the plot area in the bottom right corner
I need to manipulate the font size to make the graph look pretty



To fix these issues:
1. the y-axis should start at 0.
2. there should be a red line for outdoor_model_west and a blue line for outdoor_model, both on the same graph.
3. There should be R² and p-values for both outdoor_model_west and outdoor_modelcolured red and blue respectively. Reduce their font size at fit them in the bottom right corner of the graph.
4. There should be a label for the color red = Western Countries only

'''
