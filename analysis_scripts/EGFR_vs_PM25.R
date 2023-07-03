setwd("~/Desktop/AIIMS/letter_to_editor")

library(tidyverse)
library(rnaturalearth) # World Map Data from Natural Earth
library(sf) # Geographic Simple Features in R
library(wbstats)
library(dplyr)
library(ggplot2)
library(maps)
library(viridis)
library(RColorBrewer)
library(scales)
library(ggrepel)
library(ggpubr)


## Get input data
globocan_lc_incidence <- read.csv("data/Global_estimated_lung_cancer_incidence_rates_both_sexes.csv")
EGFR_lc <- read.csv("data/EGFR_positive_estimated_incidence.csv")
df_pol <- read.csv("data/PM2.5-air-pollution.csv")
df_clean_feul <- read.csv("data/Clean_fuel.csv")

  
# prepare Lung cancer incidence rates for 36 countries
df_EGFR <- inner_join(globocan_lc_incidence, EGFR_lc, by = "ISO_code")
df_EGFR <- subset(df_EGFR, select = -Country_name)
df_EGFR$EGFR_incidence_rate <- (df_EGFR$LC_ASR_both_sexes * df_EGFR$mEGFR_freq_overall)/100


#filter df_pol to only year 2017
df_pol <- filter(df_pol, year == 2017)
df_clean_feul <- filter(df_clean_feul, Year == 2017)

## ----Exploring data distribution-----------------
# sqrt transformation to improve color palette
df_EGFR %>%
  ggplot() +
  geom_histogram(aes(EGFR_incidence_rate)) +
  theme_minimal() +
  scale_x_log10()

## ----Download world data-------------------------
world <- ne_countries(scale="medium", returnclass="sf") %>%
  filter(admin != "Antarctica")


## ----Plot EGFR data on world map------------------------------
world_EGFR <- world %>%
  left_join(df_EGFR, by = c("iso_a3" = "ISO_code")) %>%
  ggplot() +
  geom_sf(aes(fill = EGFR_incidence_rate)) +
  scale_fill_viridis_c(
    trans = "identity",
    labels = function(x) paste0(round(x, 1))) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5),
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_text(size = rel(0.8))) +
  labs(
    title = "Estimated incidence rate of EGFR positive NSCLC (per 100,000)",
    fill = "EGFR+ NSCLC incidence per 100,000") 


## ----Plot PM 2.5 levels data on world map------------------------------
world_pol <- world %>%
  left_join(df_pol, by = c("iso_a3" = "ISO_code")) %>%
  ggplot() +
  geom_sf(aes(fill = PM2.5)) +
  scale_fill_viridis_c(
    trans = "identity",
    labels = function(x) paste0(round(x, 1))) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5),
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_text(size = rel(0.8))) +
  labs(
    fill = "PM2.5 level (ug/m3)",
    title = "Global PM2.5 levels in 2017") 

## ----Plot clean fuel data on world map------------------------------
world_clean_fuel <- world %>%
  left_join(df_clean_feul, by = c("iso_a3" = "ISO_code")) %>%
  ggplot() +
  geom_sf(aes(fill = Clean_fuels)) +
  scale_fill_viridis_c(
    trans = "identity",
    labels = function(x) paste0(round(x, 1))) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5),
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_text(size = rel(0.8))) +
  labs(
    fill = "% population using clean fuels",
    title = "Global access to clean fuels for cooking in 2017") 



## ----Scatterplot PM 2.5 levels vs EGFR+ NSCLC incidence rate----------------
options(ggrepel.max.overlaps = Inf)

df_EGFR_pol <- merge(df_EGFR, df_pol, by = "ISO_code") %>%
  select(ISO_code, Country, PM2.5, EGFR_incidence_rate)


# Create the scatter plot using ggplot2 and ggrepel
p <- ggplot(df_EGFR_pol, aes(x = PM2.5, y = EGFR_incidence_rate, label = Country)) +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(size = 6, nudge_x = 0.25, nudge_y = 0.07) +
  geom_smooth(method = "lm", se = FALSE, color = alpha("red", 0.3), formula = y ~ x) +
  
  labs(title = "Effect of outdoor pollution on EGFR driven LC",
       x = "PM2.5 levels (µg/m³) in 2017",
       y = "Estimated EGFR driven LC incidence (per 100,000)") +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.caption = element_text(hjust = 1, size = 16))

# Calculate the R value
outdoor_model <- lm(EGFR_incidence_rate ~ PM2.5, data = df_EGFR_pol)
summary(outdoor_model)

r_squared <- summary(outdoor_model)$r.squared
r_value <- round(sqrt(r_squared), 2)
p_value <- 0.487

# Add the R value to the plot
scatter_EGFR_PM2.5 <- p +
  annotate("text", x = max(df_EGFR_pol$PM2.5), y = max(df_EGFR_pol$EGFR_incidence_rate),
           label = paste0("R² = ", round(r_squared, 3), "\n", "p = ", p_value),
           hjust = 1, vjust = 1, size = 10, fontface = "bold")


## ----Scatterplot clean feul levels vs EGFR+ NSCLC incidence rate--------------

df_EGFR_fuel <- merge(df_EGFR, df_clean_feul, by = "ISO_code") %>%
  select(ISO_code, Country, Clean_fuels, EGFR_incidence_rate)

# Create the scatter plot using ggplot2 and ggrepel
q <- ggplot(df_EGFR_fuel, aes(x = Clean_fuels, y = EGFR_incidence_rate, label = Country)) +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(size = 5, nudge_x = 0.1, nudge_y = 0.1) +
  geom_smooth(method = "lm", se = FALSE, color = alpha("red", 0.3), formula = y ~ x) +
  
  labs(title = "Effect of indoor pollution on EGFR driven LC",
       x = "Percentage of household with access to clean cooking fuels in 2017",
       y = "Estimated EGFR driven LC incidence (per 100,000)") +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.caption = element_text(hjust = 1, size = 16))

# Calculate the R value
indoor_model <- lm(EGFR_incidence_rate ~ Clean_fuels, data = df_EGFR_fuel)
summary(indoor_model)

r_squared <- summary(indoor_model)$r.squared
r_value <- round(sqrt(r_squared), 2)
p_value <- 0.646

# Add the R value to the plot
scatter_EGFR_fuel <- q +
  annotate("text", x = max(df_EGFR_pol$PM2.5), y = max(df_EGFR_fuel$EGFR_incidence_rate),
             label = paste0("R² = ", round(r_squared, 3), "\n", "p = ", p_value),
             hjust = 1, vjust = 1, size = 10, fontface = "bold")


## -------Save figures-------------
ggsave("world_clean_fuel.png", world_clean_fuel)
ggsave("world_EGFR.png", world_EGFR)
ggsave("world_pol.png", world_pol)
ggsave("scatter_EGFR_fuel.png", scatter_EGFR_fuel)
ggsave("scatter_EGFR_PM2.5.png", scatter_EGFR_PM2.5)
