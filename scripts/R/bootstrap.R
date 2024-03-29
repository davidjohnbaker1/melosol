#-------------------------------------------------------------------------------
# Bootstrap Analysis
# Calculate bootstrap intervals for n-gram counts
set.seed(42, sample.kind = "Rounding")
library(viridis)
library(magrittr)
library(ggplot2)
library(dplyr)
library(stringr)

# imports in all the n-grams 
source("scripts/R/bootstrap-import.R")

# Data 
head(melosol_ngrams)

# Set Number of Replications
B <- 100000

# Compute Number of Rows
N_melosol_2gram <- nrow(melosol_ngrams)
N_essen_2gram <- nrow(essen_ngrams)

# Generate Bootstrap
generate_gram_bootstrap <- function(x, gram, B, N) {
  replicate(B, {
    notes_star <- sample(x$notes, N, replace = T)
    tstars <- mean(notes_star == gram)
    log10(tstars*100)
  })
}

#-------------------------------------------------------------------------------
# Set Up N- Grams
essen_2gram_list <- paste(unique(essen_ngrams$notes))
melosol_2gram_list <- paste(unique(melosol_ngrams$notes))

datalist_melosol <- list()
datalist_essen <- list()

#-------------------------------------------------------------------------------
# Calculate Bootstrap for MeloSol 2 Grams

# for (gram in melosol_2gram_list){
#   print(paste("working on ", gram,"running",B,"iterations"))
#   dat <- generate_gram_bootstrap(melosol_ngrams, B , gram = gram, N = nrow(melosol_ngrams))
#   datalist_melosol[[gram]] <- dat
# }
# 
# saveRDS(object = datalist_melosol, file = "data/melosol_bigram_100000_boostrap.RDS")

datalist_melosol <- readRDS(file = "data/melosol_bigram_100000_boostrap.RDS")

for (gram in essen_2gram_list){
  print(paste("working on ", gram,"running",B,"iterations"))
  dat <- generate_gram_bootstrap(essen_ngrams, B , gram = gram, N = nrow(essen_ngrams))
  datalist_essen[[gram]] <- dat
}

#saveRDS(object = datalist_essen, file = "data/esseb_bigram_100000_boostrap.RDS")

datalist_essen <- readRDS(file = "data/esseb_bigram_100000_boostrap.RDS")

#-------------------------------------------------------------------------------
# Take Bootstraps and Make into nice tidy tables 

make_table_for_plots <- function(datalist){
  
  # get and clean CIs
  confidence_intervals <- sapply(datalist, quantile, probs = c(0.025, 0.975)) %>% t()
  confidence_intervals <- confidence_intervals %>% data.frame()
  confidence_intervals$gram <- row.names(confidence_intervals)
  
  # get and clean means
  means <- sapply(datalist, mean)
  mean_transposed <- means %>% t() %>% t()
  mean_t_df <- mean_transposed %>% data.frame()
  mean_t_df$gram <- row.names(mean_t_df)

  # combine CI and means
  gg_table <- mean_t_df %>%
    left_join(confidence_intervals)
  
  # rename 
  colnames(gg_table)  <- c("statistic", "gram","lowerlimit", "upperlimit")
  return(gg_table)

}


melosol_tidy_2 <- make_table_for_plots(datalist_melosol)
melosol_tidy_2$corpus <- "MeloSol"

essen_tidy_2 <- make_table_for_plots(datalist_essen)
essen_tidy_2$corpus <- "Essen"

plot_data <- rbind(melosol_tidy_2, essen_tidy_2)


figure_5 <- plot_data %>%
  tibble() %>%
  # filter obvious context errors, only within octave
  filter(statistic > -1) %>%
  mutate(gram = str_replace_all(string = gram, "1","+1")) %>%
  filter(gram != "+9") %>%
  ggplot(aes(x = reorder(gram, statistic), y = statistic, 
             group = corpus, 
             color = corpus)) +
  geom_point(position = position_dodge(width = 1), size = 2) +
  geom_errorbar(aes(ymin = lowerlimit, ymax = upperlimit), position = position_dodge(width = 1)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x  = element_text(size=15)) +
  theme(axis.text.y  = element_text(size=15)) +
  expand_limits(y = c(-2,2)) +
  labs(title = paste0("Melodic Interval Bigrams with Confidence Interval"),
       subtitle = paste("100,000 Bootstrap Iterations"),
       x = "2-gram",
       y = "Log Mean Percent",
       color = "Corpus") +
  scale_color_viridis(discrete = TRUE, begin = .25, end = 1)

figure_5

# Figure 5:                                                                       
# Why do the points not align in rows?  (answered in text)                                
# How is the vertical axis ordered? Does not seem to be by either Essen or Meolosol. (relative appearance)
# What is log mean percent? If percent < 1, log should be < 0?                   
# What are these 2-grams? Is it the difference between consecutive intervals? Or the interval between consecutive pitc (put in text)
# The label "1" should be "+1".

ggsave(filename = "img/Figure_5.png", figure_5, height = 12, width = 24, units = "cm")
