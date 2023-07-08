library(tidyverse)
library(ggplot2)
library(ggrepel)

setwd("~/Desktop/AIIMS/letter_to_editor")

western_countries <- c('AUS', 'AUT', 'CAN', 'CZE', 'DEU',
                       'ESP', 'FIN', 'FRA', 'GBR', 'GRC',
                       'ITA', 'LTU', 'NLD', 'NOR', 'POL',
                       'PRT', 'SVK', 'SWE', 'USA')

## Get input data
globocan_lc_incidence <- read.csv("data/Global_estimated_lung_cancer_incidence_rates_both_sexes.csv")
EGFR_lc <- read.csv("data/EGFR_positive_estimated_incidence.csv")
df_pol <- read.csv("data/PM2.5-air-pollution.csv")
df_clean_fuel <- read.csv("data/Clean_fuel.csv")

# Prepare Lung cancer incidence rates for 36 countries
df_EGFR <- inner_join(globocan_lc_incidence, EGFR_lc, by = "ISO_code")
df_EGFR <- select(df_EGFR, -Country_name)
df_EGFR$EGFR_incidence_rate <- (df_EGFR$LC_ASR_both_sexes * df_EGFR$mEGFR_freq_overall) / 100

# Filter df_pol to only year 2017
df_pol <- filter(df_pol, year == 2017)
df_clean_fuel <- filter(df_clean_fuel, Year == 2017)

# Get final dataframes for modeling
df_EGFR_pol <- merge(df_EGFR, df_pol, by = "ISO_code") %>%
  select(ISO_code, Country, PM2.5, EGFR_incidence_rate)

west_df_EGFR_pol <- df_EGFR_pol[df_EGFR_pol$ISO_code %in% western_countries, ]

df_EGFR_fuel <- merge(df_EGFR, df_clean_fuel, by = "ISO_code") %>%
  select(ISO_code, Country, Clean_fuels, EGFR_incidence_rate)

west_df_EGFR_fuel <- df_EGFR_fuel[df_EGFR_fuel$ISO_code %in% western_countries, ]

## Scatterplot PM 2.5 levels vs EGFR+ NSCLC incidence rate
options(ggrepel.max.overlaps = Inf)

# Create the scatter plot for outdoor pollution
p <- ggplot(df_EGFR_pol, aes(x = PM2.5, y = EGFR_incidence_rate, label = Country)) +
  geom_point(size = 3, color = ifelse(df_EGFR_pol$ISO_code %in% western_countries, "red", "blue")) +
  geom_text_repel(size = 6, nudge_x = 0.25, nudge_y = 0.07) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, color = alpha("black", 0.5), formula = y ~ x) +
  geom_smooth(data = west_df_EGFR_pol, method = "lm", se = FALSE, color = alpha("red", 0.7), formula = y ~ x + I(x > 5), fullrange = TRUE) +
  labs(title = "Effect of outdoor pollution on EGFR driven LC",
       x = "PM2.5 levels (µg/m³) in 2017",
       y = "Estimated EGFR driven LC incidence (per 100,000)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size =   32, face = "bold"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.caption = element_text(hjust = 1, size = 16)) 

# Calculate the R value for df_EGFR_pol
outdoor_model <- lm(EGFR_incidence_rate ~ PM2.5, data = df_EGFR_pol)
r_squared_out <- summary(outdoor_model)$r.squared
p_value_out <- summary(outdoor_model)$coef[2, 4]  # Extract the p-value

# Calculate the R value for west_df_EGFR_pol
outdoor_model_west <- lm(EGFR_incidence_rate ~ PM2.5, data = west_df_EGFR_pol)
r_squared_west <- summary(outdoor_model_west)$r.squared
p_value_west <- summary(outdoor_model_west)$coef[2, 4]  # Extract the p-value

# Add the R-squared and p-value to the plot
scatter_EGFR_PM2.5 <- p +
  annotate("text", x = max(df_EGFR_pol$PM2.5), y = max(df_EGFR_pol$EGFR_incidence_rate),
           label = paste0("All Countries\nR² = ", round(r_squared_out, 3), "\np-value = ", round(p_value_out, 3)),
           hjust = 1, vjust = 3.5, size = 4, fontface = "bold", color = "black") +
  annotate("text", x = max(df_EGFR_pol$PM2.5), y = max(df_EGFR_pol$EGFR_incidence_rate),
           label = paste0("Western Countries only\nR² = ", round(r_squared_west, 3), "\np-value = ", round(p_value_west, 3)),
           hjust = 1, vjust = 2, size = 4, fontface = "bold", color = "red")

# Display the scatter plot with R-squared, p-values, and updated settings
scatter_EGFR_PM2.5

# Create the scatter plot for indoor pollution
q <- ggplot(df_EGFR_fuel, aes(x = Clean_fuels, y = EGFR_incidence_rate, label = Country)) +
  geom_point(size = 3, color = ifelse(df_EGFR_pol$ISO_code %in% western_countries, "red", "blue")) +
  geom_text_repel(size = 5, nudge_x = 0.1, nudge_y = 0.1) +
  geom_smooth(method = "lm", se = FALSE, color = alpha("black", 0.5), formula = y ~ x) +
  labs(title = "Effect of indoor pollution on EGFR driven LC",
       x = "Percentage of households with access to clean cooking fuels in 2017",
       y = "Estimated EGFR driven LC incidence (per 100,000)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.caption = element_text(hjust = 1, size = 16))

# Calculate the R value for df_EGFR_fuel
indoor_model <- lm(EGFR_incidence_rate ~ Clean_fuels, data = df_EGFR_fuel)
r_squared_in <- summary(indoor_model)$r.squared
p_value_in <- summary(indoor_model)$coef[2, 4]  # Extract the p-value

scatter_EGFR_fuel <- q +
  annotate("text", x = max(df_EGFR_fuel$Clean_fuels), y = max(df_EGFR_fuel$EGFR_incidence_rate),
           label = paste0("All Countries\nR² = ", round(r_squared_in, 3), "\np-value = ", round(p_value_in, 3)),
           hjust = 1, vjust = 0.6, size = 4, fontface = "bold", color = "black")

# Display the scatter plot with R-squared, p-values, and updated settings
scatter_EGFR_fuel


