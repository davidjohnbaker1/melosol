#################################################################################
# Correlation Analysis 
#------------------------------------------------------------------------------
# Correlation of 3,4,5,7,9,11 grams

# imports in all the n-grams 
source("scripts/R/bootstrap-import.R")

# trigrams

correlate_ngrams <- function(melosol_grams, essen_grams){
  
  melosol_table <- melosol_grams %>%
    tibble() %>%
    count(notes) %>%
    summarise(notes,
              percents = n/sum(n)) %>%
    arrange(desc(percents))
  
  essen_table <- essen_grams %>%
    tibble() %>%
    count(notes) %>%
    summarise(notes,
              percents = n/sum(n)) %>%
    arrange(desc(percents))
  
  cor_table <- melosol_table %>%
    full_join(essen_table, by = "notes") %>%
    select(-notes) %>%
    cor(use = "pairwise.complete.obs", method = "spearman")
  
  print(paste("The spearman correlation is:", cor_table[2]))
  
}

correlate_ngrams(melosol_ngrams, essen_ngrams)
correlate_ngrams(melosol_trigrams, essen_trigrams)
correlate_ngrams(melosol_quadgrams, essen_quadgrams)
correlate_ngrams(melosol_quintgrams, essen_quintgrams)
correlate_ngrams(melosol_heptgrams, essen_heptgrams)
correlate_ngrams(melosol_ninegrams, essen_nongrams)
correlate_ngrams(melosol_elevengrams, essen_hendegrams)

cor_data_for_plot <- tribble(
  ~gram, ~spearman,
  " 2-gram", 0.9838,
  " 3-gram", 0.9391,
  " 4-gram", 0.8017,
  " 5-gram", 0.6280,
  " 7-gram", 0.3999,
  " 9-gram", 0.3336,
  "11-gram", 0.2765)

figure_6 <- cor_data_for_plot %>%
  mutate(spearman_label = as.character(format(round(spearman,3), nsmall = 3))) %>%
  ggplot(aes(x = gram, y = spearman, label = spearman_label)) +
  geom_point() +
  geom_text(nudge_x = .3, size = 5) +
  expand_limits(y = c(0,1)) +
  theme_minimal() +
  labs(title = "Correlation Between MeloSol and Essen n-grams",
       y = expression(paste("Spearman's ", rho)),
       x = "Lenght of Melodic Interval Measured in Half Steps" )

ggsave("img/Figure_6.png",height = 12, width = 24, units = "cm")
