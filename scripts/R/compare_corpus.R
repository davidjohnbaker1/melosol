#--------------------------------------------------
# Comparison of Corpora
#--------------------------------------------------
# NOTE THIS SCRIPT DOES NOT WORK YET AND
# NEEDS TO BE MODIFIED FOR DATA REPORT PAPER 
#--------------------------------------------------
library(ggplot2)
library(scales)
library(readr)
library(dplyr)
library(magrittr)
library(cowplot)
library(viridis)

#--------------------------------------------------
# Import Feature Data
melosol_features <- read_csv("corpus/fantastic/melosol_features.csv")
densmore_features <- read_csv("corpus/fantastic/densmore_features.csv")
essen_features <- read_csv("corpus/fantastic/esssen_features.csv")
#--------------------------------------------------
# Magnitude comparision 
nrow(essen_features) / nrow(melosol_features)
nrow(densmore_features) / nrow(melosol_features)


# Import Dist Data
# !! Maybe Delete!!
# melosol_krum <- read_csv("corpus/symbolic/krn/melosol_deg.csv")
# euro_krum <- read_csv("corpus/symbolic/krn/essen/euro_degs.csv")
# asia_krum <- read_csv("corpus/symbolic/krn/essen/asia_deg.csv")

# 
# melosol_krum$dataset <- "MeloSol"
# euro_krum$dataset <- "Euro"
# asia_krum$dataset <- "Asia"
# 
# krum_data <- rbind(melosol_krum, euro_krum, asia_krum)

melosol_features$Country <- "Western"
densmore_features$Country <- "North America"

#--------------------------------------------------
# Add Dataset Identifier Tags
melosol_features$dataset <- "MeloSol"
densmore_features$dataset <- "Densmore"
essen_features$dataset <- "Essen"

#--------------------------------------------------
# Create Origin
melosol_features$origin <- "Western Classical"
densmore_features$origin <- "Native American"

# Subset Asia From Essen
# ashsham1.krn  india01.krn   java01.krn    nippon01.krn  turkiye1.krn
# han/    natmin/ shanxi/ xinhua/
essen_features$origin <- "NULL"
#--------------------------------------------------
# Drop Countries where not Enough
# arabic (1), mexico (4), brasil (1), canada (4), usa (10)

essen_features %>%
  filter(Country != "arabic") %>%
  filter(Country != "mexico") %>%
  filter(Country != "brasil") %>%
  filter(Country != "canada") %>%
  filter(Country != "usa") -> essen_features

#--------------------------------------------------
# Asia Origin
essen_features[essen_features$Country=="han",]$origin <- "Asian"
essen_features[essen_features$Country=="natmn",]$origin <- "Asian"
essen_features[essen_features$Country=="shanx",]$origin <- "Asian"
essen_features[essen_features$Country=="china",]$origin <- "Asian"
essen_features[essen_features$Country=="han",]$origin <- "Asian"
essen_features[essen_features$Country=="hanb",]$origin <- "Asian"
essen_features[essen_features$Country=="ashsham",]$origin <- "Asian"

#--------------------------------------------------
# Eurpean Origin 
essen_features[essen_features$origin != "Asian",]$origin <- "Europe"

names(densmore_features)
#--------------------------------------------------
# Rename for Merge
essen_features %>%
  rename(melody_id = folksong) -> essen_features

densmore_features %>%
  rename(melody_id = file.id) -> densmore_features

melosol_features %>%
  rename(melody_id = file.id) -> melosol_features

essen_features %>%
  select(-Country.ID, -Region) -> essen_features

rbind(densmore_features, melosol_features, essen_features) -> melody_features

melody_features %>%
  rename(Origin = origin) -> melody_features

#--------------------------------------------------
# Create Descriptives

melody_features %>%
  group_by(dataset, Origin) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = dataset, y = n, fill = Origin)) +
  geom_bar(stat = 'identity') +
  theme_minimal() + 
  labs(title = "Breakdown of Corpora", x = "Corpus", y = "Frequency Count") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE) -> corpora_size_comparison

corpora_size_comparison

melody_features %>%
  select(Origin, len, dataset) %>%
  ggplot(aes(x = len, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_freqpoly() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Length Density", x = "Number of Notes", y = "Frequency Count") -> corpora_len_comparision

corpora_len_comparision

melody_features %>%
  group_by(Origin) %>%
  summarise(median_len = median(p.range))

melody_features %>%
  select(Origin, p.range, dataset) %>%
  ggplot(aes(x = p.range, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_freqpoly() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Range Density", x = "Range in Semitones", y = "Frequency Count") -> corpora_range_comparison

corpora_range_comparison

melody_features %>%
  select(Origin, note.dens, dataset) %>%
  ggplot(aes(x = note.dens, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Note Density", y = "Density", x = "Number of Notes Per Second") -> corpora_ndensity_comparison

corpora_ndensity_comparison

plot_grid(corpora_range_comparison, 
          corpora_len_comparision, 
          corpora_ndensity_comparison, 
          corpora_size_comparison) -> comparative_descriptive_panel

comparative_descriptive_panel

ggsave(filename = "img/Figure_2.png", height = 20, width = 30, units = "cm")
# Saving 11.6 x 8 in image

# Make Huron Panel Grid 

melody_features %>%
  ggplot(aes(x = i.entropy, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Interval Entropy Density", 
       subtitle = "Variant of Shannon Entropy on Discrete Intervals", 
       x = "Entropy", 
       y = "Kernel Density Estimate") -> corpora_ientropy_comparision

corpora_ientropy_comparision

melody_features %>%
  ggplot(aes(x = tonalness, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Tonalness Density", 
       y = "Kernel Density Estimate", 
       subtitle = "Tonalness measures magnitude of the highest correlation with\nsingle key", 
       x = "Tonalness") -> corpora_tonalness_comparision

melody_features %>%
  ggplot(aes(x = tonal.clarity, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Tonal Clarity Density", 
       x = "Tonal Clarity", 
       y = "Kernel Density Estimate", 
       subtitle = "Ratio between the magnitude of the highest correlation\nin  the tonality vector and  the  second highest correlation.\nInspired by Temperly (2007)") -> corpora_tonalclarity_comparision

melody_features %>%
  ggplot(aes(x = tonal.spike, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Tonal Spike Density", y = "Kernel Density Estimate",  
       subtitle = "Magnitude of the highest correlation divided by the sum\nof all correlation values", 
       x = "Tonalspike") -> corpora_tonalspike_comparision

corpora_tonalclarity_comparision
corpora_tonalness_comparision
corpora_tonalspike_comparision

melody_features %>%
  ggplot(aes(x = d.entropy, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Durational Entropy Density",  y = "Kernel Density Estimate", 
       subtitle = "Variant of Shannon entropy based on note durations", 
       x = "Entropy Calculation") -> corpora_dentropy_comparision

corpora_dentropy_comparision

melody_features %>%
  ggplot(aes(x = step.cont.glob.var, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Step Countour Global Variation", 
       subtitle = "Standard Deviation of Melody Contour Vector", x = "Standard Deviation",  y = "Kernel Density Estimate") -> corpora_scgv_comparision

corpora_scgv_comparision

plot_grid(ncol =  2, corpora_ientropy_comparision, corpora_dentropy_comparision,
          corpora_tonalclarity_comparision,
          corpora_tonalness_comparision,
          corpora_tonalspike_comparision,
          corpora_scgv_comparision) -> corpora_emergent 

corpora_emergent
ggsave(filename = "img/Figure_3.png", corpora_emergent, width = 30, height = 20, unit = "cm")
# Saving 13.6 x 8 in image

#--------------------------------------------------
# Redo Huron 

melody_features %>%
  select(h.contour, Origin) %>%
  group_by(Origin, h.contour) %>%
  summarise(n = n()) %>%
  group_by(Origin) %>% 
  mutate(perc=n/sum(n)) %>%
  ggplot(aes(x = h.contour, y = perc, fill = Origin)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Origin) +
  theme_minimal() +
  labs(title = "Huron Contour Class", 
       x = "Contour Class",
       y = "") +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() -> corpora_contour_distribution 

corpora_contour_distribution

ggsave(filename = "img/Figure_4.png", corpora_contour_distribution, height = 20, width = 30, units = "cm")
# Saving 13.6 x 8 in image
#--------------------------------------------------
# Plots this Scritp Makes

comparative_descriptive_panel
corpora_emergent
corpora_contour_distribution
