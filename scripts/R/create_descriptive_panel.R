#---------------------------------------------------
# Create Descriptive Statistic Plots 
#---------------------------------------------------
library(readr)
library(ggplot2)
library(magrittr)
library(dplyr)
library(stringr)
library(cowplot)
library(viridis)
#----------------------------------------------------
meta <- read_csv("corpus/metadata/melosol_metadata.csv") 
fantastic_computations <- read_csv("corpus/fantastic/melosol_features.csv")

meta

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
  filter(major_minor == "MAJOR") %>%
  mutate(Key = str_replace_all(string = Key, "-","\u266D")) %>%
  mutate(Key = factor(Key, levels = c("C","G","D","A","E","B","F#","G♭","C#","D♭","A♭","E♭", "B♭","F"))) %>%
  ggplot(aes(x = Key)) +
  geom_bar(aes(fill = `!!!Section`)) +
  theme_minimal() +
  theme(axis.text=element_text(size=10)) +
  labs(title = "Key Distribution", 
       x = "Key",
       y = "Frequency Count", fill = "Section") +
  coord_flip() +
  theme(legend.position = "none") + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~major_minor) +
  scale_fill_viridis(discrete = TRUE, begin = 0, end = .8)   -> melosol_key_distribution_major 

melosol_key_distribution_major 
#--------------------------------------------------

melosol_features %>%
  filter(`!!!Section` != "V") %>%
  filter(major_minor == "MINOR") %>%
  mutate(Key = str_replace_all(string = Key, "-","\u266D")) %>%
  mutate(Key = factor(Key, levels = c("c","g","d","a","e","b","f#","g♭","c#","d♭","a♭","e♭", "b♭","f"))) %>%
  ggplot(aes(x = Key)) +
  geom_bar(aes(fill = `!!!Section`)) +
  theme_minimal() +
  theme(axis.text=element_text(size=10)) +
  labs(title = "Key Distribution", 
       x = "Key",
       y = "Frequency Count", fill = "Section") +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~major_minor) +
  expand_limits(y = c(0,60)) +
  scale_fill_viridis(discrete = TRUE, begin = 0, end = .8)   -> melosol_key_distribution_minor 

melosol_key_distribution_minor

major_minor_plot <- plot_grid(melosol_key_distribution_major, melosol_key_distribution_minor)
major_minor_plot

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
       y =  "Frequency Count",
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
       y = "Frequency Count", fill = "Section") -> melosol_range_distribution 

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
       y = "Frequency Count", fill = "Section") +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip() -> melosol_contour_distribution 

melosol_contour_distribution

#--------------------------------------------------
# Make Panel 
plot_grid(major_minor_plot, melosol_len_distribution, 
          melosol_range_distribution, melosol_contour_distribution) -> melosol_descript_panel

melosol_descript_panel
ggsave(filename = "img/Figure_1.png", melosol_descript_panel, height = 20, width = 30, units = "cm")
# Saving 12.4 x 8 in image
#--------------------------------------------------

