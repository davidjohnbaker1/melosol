# Import Data and Libraries
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
# Import -- MeloSol
## Bigrams
melosol_ngrams <- read.delim("corpus/ngrams/bi-gram.tsv", header = FALSE)
names(melosol_ngrams) <- "notes"
melosol_ngrams <- melosol_ngrams %>%
  filter(notes != "1")

melosol_trigrams <- read.delim("corpus/ngrams/melosol-tri-gram.tsv", header = FALSE)
names(melosol_trigrams) <- "notes"

melosol_quadgrams <- read.delim("corpus/ngrams/melosol-four-gram.tsv", header = FALSE)
names(melosol_quadgrams) <- "notes"

melosol_quintgrams <- read.delim("corpus/ngrams/melosol-five-gram.tsv", header = FALSE)
names(melosol_quintgrams) <- "notes"

melosol_heptgrams <- read.delim("corpus/ngrams/melosol-seven-gram.tsv", header = FALSE)
names(melosol_heptgrams) <- "notes"

melosol_ninegrams <- read.delim("corpus/ngrams/melosol-nine-gram.tsv", header = FALSE)
names(melosol_ninegrams) <- "notes"

melosol_elevengrams <- read.delim("corpus/ngrams/melosol-eleven-gram.tsv", header = FALSE)
names(melosol_elevengrams) <- "notes"

#-------------------------------------------------------------------------------
# Essen Data 
essen_ngrams <- read.delim("corpus/ngrams/bi-gram-essen.tsv", header = FALSE)
names(essen_ngrams) <- "notes"
essen_ngrams <- essen_ngrams %>%
  filter(notes != "1") %>%
  filter(notes != "\\{1")

essen_ngrams <- essen_ngrams %>% 
  tibble() %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\{")) %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\}"))

essen_trigrams <- read.delim("corpus/ngrams/tri-gram-essen.tsv")
names(essen_trigrams) <- "notes"

essen_trigrams <- essen_trigrams %>% 
  tibble() %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\{")) %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\}"))


essen_quadgrams <- read.delim("corpus/ngrams/quad-gram-essen.tsv")
names(essen_quadgrams) <- "notes"

essen_quadgrams <- essen_quadgrams %>% 
  tibble() %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\{")) %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\}"))


essen_quintgrams <- read.delim("corpus/ngrams/quint-gram-essen.tsv")
names(essen_quintgrams) <- "notes"

essen_quintgrams <- essen_quintgrams %>% 
  tibble() %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\{")) %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\}"))

essen_heptgrams <- read.delim("corpus/ngrams/hept-gram-essen.tsv")
names(essen_heptgrams) <- "notes"

essen_heptgrams <- essen_heptgrams %>% 
  tibble() %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\{")) %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\}"))

essen_nongrams <- read.delim("corpus/ngrams/non-gram-essen.tsv")
names(essen_nongrams) <- "notes"

essen_nongrams <- essen_nongrams %>% 
  tibble() %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\{")) %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\}"))

essen_hendegrams <- read.delim("corpus/ngrams/hende-gram-essen.tsv")
names(essen_hendegrams) <- "notes"

essen_hendegrams <- essen_hendegrams %>% 
  tibble() %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\{")) %>%
  mutate(notes = str_remove_all(string = notes, pattern = "\\}"))

