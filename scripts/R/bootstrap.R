# Bootstrap Analysis
# Calculate bootstrap intervals for n-gram counts
# install.packages("boot", dep = TRUE)
library(boot)
library(patchwork)
library(ggplot2)
library(broom)
library(stringr)
library(dplyr)
library(magrittr)
set.seed(23423)
#-----------------------------------------
# 1. Show that n-grams are reliable 
# 2. Show n-grams correlate with Essen 
#-----------------------------------------

#-----------------------------------------#
# Show n- grams are reliable 
# * Code to bootstrap proportions of 1,3,5,7 grams
# * Calculates t* and CIs 
# * Run on MeloSol, Essen, Bach? 
# * Plot results next to each other 
#------------------------------------------#

#--------------------------------------------
# Import 
## bigrams
melosol_ngrams <- read.delim("corpus/ngrams/bi-gram.tsv", header = FALSE)
names(melosol_ngrams) <- "notes"
melosol_ngrams <- melosol_ngrams %>%
  filter(notes != "1")

melosol_trigrams <- read.delim("corpus/ngrams/melosol-tri-gram.tsv", header = FALSE)
names(melosol_trigrams) <- "notes"

essen_ngrams <- read.delim("corpus/ngrams/bi-gram-essen.tsv", header = FALSE)
names(essen_ngrams) <- "notes"
essen_ngrams <- essen_ngrams %>%
  filter(notes != "1") %>%
  filter(notes != "\\{1")

essen_ngrams <- essen_ngrams %>% 
  tibble() %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\{")) %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\}"))

essen_3_grams <- read.delim("corpus/ngrams/tri-gram-essen.tsv")
names(essen_3_grams) <- "notes"

essen_3_grams <- essen_3_grams %>% 
  tibble() %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\{")) %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\}"))

#bach_soprano_ngrams <- NULL

# Function for boot()
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
bootstraps_to_run <- 10
#---------
# melosol 
bootstrap_analysis <- boot(melosol_ngrams, notes_bootstrap, R = bootstraps_to_run)
bs_ms_3 <- boot(melosol_trigrams, notes_bootstrap, R= bootstraps_to_run)


melosol_tidy <- tidy(bootstrap_analysis, conf.int = TRUE, conf.method = "perc")
bs_ms_3_tidy <- tidy(bs_ms_3, conf.int = TRUE, conf.method = "perc")

melosol_tidy %>%
  arrange(desc(statistic))

melosol_plot <- melosol_tidy %>%
  ggplot(aes(x = reorder(term, statistic), y = statistic)) +
  geom_point() +
  geom_errorbar(aes(ymin = statistic - conf.low, ymax = statistic + conf.high)) +
  coord_flip() + 
  theme_minimal() +
  labs(title = "MeloSol Mint b-grams",
       x = "Melodic Interval",
       y = "Statistic") 

melosol_plot

melosol_plot_3 <- bs_ms_3_tidy %>%
  filter(statistic >= .01) %>%
  ggplot(aes(x = reorder(term, statistic), y = statistic)) +
  geom_point() +
  geom_errorbar(aes(ymin = statistic - conf.low, ymax = statistic + conf.high)) +
  coord_flip() + 
  theme_minimal() +
  labs(title = "MeloSol Mint b-grams",
       x = "Melodic Interval",
       y = "Statistic") 

melosol_plot_3


#-----------------------------------------------------------------------------
# Essen 
bootstrap_analysis_essen <- boot(essen_ngrams, notes_bootstrap, R = bootstraps_to_run)
bs_essen_3 <- boot(essen_3_grams, notes_bootstrap, R = bootstraps_to_run)

essen_tidy <- tidy(bootstrap_analysis_essen, conf.int = TRUE, conf.method = "perc")

essen_3_tidy <- tidy(essen_3_grams, conf.int = TRUE, conf.method = "perc")

essen_plot <- essen_tidy %>%
  filter(statistic >= .0001) %>%
  ggplot(aes(x = reorder(term, statistic), y = statistic)) +
  geom_point() +
  geom_errorbar(aes(ymin = statistic - conf.low, ymax = statistic + conf.high)) +
  coord_flip() + 
  theme_minimal() +
  labs(title = "Essen Mint b-grams",
       x = "Melodic Interval",
       y = "Statistic") 

essen_plot

essen_plot_3 <- essen_3_tidy %>%
  filter(statistic >= .0001) %>%
  ggplot(aes(x = reorder(term, statistic), y = statistic)) +
  geom_point() +
  geom_errorbar(aes(ymin = statistic - conf.low, ymax = statistic + conf.high)) +
  coord_flip() + 
  theme_minimal() +
  labs(title = "Essen Mint b-grams",
       x = "Melodic Interval",
       y = "Statistic") 

essen_plot_3



#----------------------------------------------
melosol_plot + essen_plot

# -----------------------------
# Correlational Analysis 

melosol_tidy$dataset <- "melosol"
essen_tidy$dataset <- "essen"

melosol_tidy
essen_tidy

melosol_tidy %>%
  full_join(essen_tidy, by = "term") %>%
  filter(term != "P1") %>%
  select(statistic.x, statistic.y) %>%
  cor(use = "pairwise.complete.obs",method = "spearman")


