#---------------------------------------------------
# Create Descriptive Statistic Plots 
#---------------------------------------------------
library(tidyverse)
library(cowplot)
library(viridis)
#----------------------------------------------------
meta <- read_csv("corpus/metadata/original_metadata.csv") 
fantastic_computations <- read_csv("corpus/fantastic/melosol_features.csv")

melosol_metadata

fantastic_computations

#---------------------------------------------------
# Add on Major and Minor to Metadata 

meta <- meta %>%
  mutate(major_minor = ifelse(str_detect(meta$Key, pattern = "[A-G]"), "MAJOR","MINOR"))

fantastic_computations <- fantastic_computations %>%
  rename(Filename = file.id)

melosol_features <- meta %>%
  left_join(fantastic_computations)


#--------------------------------------------------
# Key Distribution Plot

# !!! Split Major and Minor 
# !!! Reorder by Circle of Fifths 

melosol_features %>%
  filter(`!!!Section` != "V") %>%
  ggplot(aes(x = Key)) +
  geom_bar(aes(fill = `!!!Section`)) +
  theme_minimal() +
  theme(axis.text=element_text(size=10)) +
  labs(title = "Key Distribution", 
       x = "Key",
       y = "", fill = "Section") +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~major_minor) +
  scale_fill_viridis(discrete = TRUE, begin = 0, end = .8)   -> melosol_key_distribution 

melosol_key_distribution
#--------------------------------------------------
# Length Plot
melosol_features %>%
  ggplot(aes(x = len)) +
  geom_bar(aes(fill = `!!!Section`)) +
  theme(legend.position = "none") +
  theme_minimal() +
  guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE) + 
  labs(title = "Length Distribution", 
       x = "Number of Notes",
       y =  "",
       fill = "Section") -> melosol_len_distribution 

melosol_len_distribution
#--------------------------------------------------
# Key Distribution Plot

melosol_features %>%
  ggplot(aes(x = p.range)) +
  geom_bar(aes(fill = `!!!Section`)) +
  theme_minimal() +
  guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Range Distribution", 
       x = "Range in Semitones",
       y = "", fill = "Section") -> melosol_range_distribution 

melosol_range_distribution
#--------------------------------------------------
# Key Distribution Plot

melosol_features %>%
  filter(h.contour != "NA") %>%
  ggplot(aes(x = h.contour)) +
  geom_bar(aes(fill = `!!!Section`)) +
  theme_minimal() +
  labs(title = "Huron Contour Class", 
       x = "Contour Class",
       y = "", fill = "Section") +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip() -> melosol_contour_distribution 

melosol_contour_distribution

#--------------------------------------------------
# Make Panel 
plot_grid(melosol_key_distribution, melosol_len_distribution, 
          melosol_range_distribution, melosol_contour_distribution) -> melosol_descript_panel

melosol_descript_panel
# ggsave(filename = "img/descriptive_panel.png", melosol_descript_panel)
# Saving 12.4 x 8 in image
#--------------------------------------------------


