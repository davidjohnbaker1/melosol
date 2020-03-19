#--------------------------------------------------
# Corpus Analysis
#--------------------------------------------------
# NOTE THIS SCRIPT DOES NOT WORK YET AND
# NEEDS TO BE MODIFIED FOR DATA REPORT PAPER 
#--------------------------------------------------
library(tidyverse)
library(cowplot)
library(viridis)

meta <- read_csv("corpus/symbolic/krn/melosol/berk_melo_meta.csv") 
fantastic_computations <- read_csv("corpus/symbolic/Melosol_Features.csv")
#--------------------------------------------------
# Make Major Minor
meta <- meta %>%
  mutate(major_minor = ifelse(str_detect(meta$Key, pattern = "[A-G]"), "MAJOR","MINOR"))

fantastic_computations <- fantastic_computations %>%
  rename(Filename = file.id)

melosol_features <- meta %>%
  left_join(fantastic_computations)

#--------------------------------------------------
# Key Distribution Plot
melosol_features %>%
  ggplot(aes(x = Key)) +
  geom_bar(aes(fill = `!!! Section`)) +
  theme_minimal() +
  theme(axis.text=element_text(size=6)) +
  guides(fill=FALSE) +
  labs(title = "Key Distribution", 
       x = "Key",
       y = "", fill = "Section") +
  scale_fill_viridis(discrete = TRUE) +
  facet_wrap(~major_minor) -> melosol_key_distribution 

melosol_key_distribution
#--------------------------------------------------
# Length Plot
melosol_features %>%
  ggplot(aes(x = len)) +
  geom_bar(aes(fill = `!!! Section`)) +
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
  geom_bar(aes(fill = `!!! Section`)) +
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
  geom_bar(aes(fill = `!!! Section`)) +
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

# ggsave(filename = "document/img/melosol_descript_panel.png", melosol_descript_panel)
#--------------------------------------------------
