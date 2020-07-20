# Bootstrap Analysis
# Calculate bootstrap intervals for n-gram counts

notes_bootstrap <- function(d, i){
  # get global set
  global_set <- d %>% distinct()

  # take random rows 
  sampler <- d[i, , drop = FALSE]
  
  proportion_table <- sampler %>%
    count(.data$notes) %>%
    mutate(proportion = n/sum(n)) %>%
    ungroup()
  
  # combine with full set to turn NAs to 0s
  combined_table <- proportion_table %>% full_join(global_set)
  final_table <- combined_table %>% 
    select(-n) %>%
    mutate(proportion = if_else(is.na(proportion),0,proportion))
  
  output <- setNames(final_table$proportion, final_table$notes)
  
  return(output)
  
}

#----------------------------------------
# Bootstrap Proportions
bootstraps_to_run <- 1000
#---------
# melosol 
# bootstrap_analysis <- boot(melosol_ngrams, notes_bootstrap, R = bootstraps_to_run)
# saveRDS(bootstrap_analysis, "data/melosol_boostrap.RDS")

melosol_tidy <- tidy(bootstrap_analysis, conf.int = TRUE, conf.method = "perc")

melosol_tidy %>%
  arrange(desc(statistic)) %>%
  print(n = 100)

melosol_plot <- melosol_tidy %>%
  filter(statistic >= .001) %>%
  ggplot(aes(x = reorder(term, statistic), y = statistic)) +
  geom_point() +
  geom_errorbar(aes(ymin = statistic - conf.low, ymax = statistic + conf.high)) +
  coord_flip() + 
  theme_minimal() +
  labs(title = "MeloSol 2-grams",
       x = "Melodic Interval",
       y = "Statistic") 

melosol_plot

#-----------------------------------------------------------------------------
# Essen 
# bootstrap_analysis_essen <- boot(essen_ngrams, notes_bootstrap, R = bootstraps_to_run)
# saveRDS(bootstrap_analysis_essen, "data/essen_boostrap.RDS")

essen_tidy <- tidy(bootstrap_analysis_essen, conf.int = TRUE, conf.method = "perc")

essen_plot <- essen_tidy %>%
  filter(statistic >= .0001) %>%
  ggplot(aes(x = reorder(term, statistic), y = statistic)) +
  geom_point() +
  geom_errorbar(aes(ymin = statistic - conf.low, ymax = statistic + conf.high)) +
  coord_flip() + 
  theme_minimal() +
  labs(title = "Essen 2-grams",
       x = "Melodic Interval",
       y = "Statistic") 

essen_plot




#----------------------------------------------
melosol_plot + essen_plot

# -----------------------------
# Correlation Analysis 

melosol_tidy$dataset <- "melosol"
essen_tidy$dataset <- "essen"

melosol_tidy
essen_tidy

melosol_tidy %>%
  full_join(essen_tidy, by = "term") %>%
  filter(term != "P1") %>%
  select(statistic.x, statistic.y) %>%
  cor(use = "pairwise.complete.obs",method = "spearman")
#------------------------------------------------------------------------------
# Correlation of 3,4,5,7,9,11 grams

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
