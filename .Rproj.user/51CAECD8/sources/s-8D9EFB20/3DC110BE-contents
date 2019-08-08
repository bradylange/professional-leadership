# Developer: Brady Lange
# Date: 04/16/2019
# Description: Calculates Term Frequency and Inverse Document Frequency 
# (TF-IDF).

# Set-up workspace
graphics.off()
rm(list = ls())

# Load libraries
library(tidyverse)
library(tidytext)

# =============================================================================
# Term Frequency and Inverse Document Frequency (TF-IDF)
# =============================================================================
# -----------------------------------------------------------------------------
# Term Frequency (tf):
# -----------------------------------------------------------------------------
# How frequently a word occurs in a document.

# -----------------------------------------------------------------------------
# Inverse Document Frequency (idf): 
# -----------------------------------------------------------------------------
# Decreases the weight for commonly used words and increases the 
# weight for words that are not used very much in a collection of documents.

# -----------------------------------------------------------------------------
# Term Frequency-Inverse Document Frequency (tf_idf): 
# -----------------------------------------------------------------------------
# Frequency of a term adjusted for how rarely it is used. Measures how 
# important a word is to a document in a collection of a documents.

# Convert job descriptions into tokens and count the frequency per job title
job_words <- jobs %>%
  unnest_tokens(tokens, description) %>%
  count(title, tokens, sort = T)

# Check if correct
dim(unique(job_words$tokens)) == dim(job_words)
job_words %>% 
  group_by(title, tokens) %>%
  unique()

# Total the amount of tokens per job title
total_words <- job_words %>%
  group_by(title) %>%
  summarize(total = sum(n)) %>%
  arrange(desc(total))

# Combine specific tokens per job title and total tokens per job title
job_words <- left_join(job_words, total_words)

# TF-IDF 
tf_idf <- job_words %>%
  bind_tf_idf(tokens, title, n)

# Leadership terms - reference (https://www.thesaurus.com/browse/leadership)
leadership_dict <- tibble(words = c("leadership", "leader", "lead", 
                                    "supervise", "supervisor", 
                                    "manager", "manage", "administration", 
                                    "administrator", "authority", "control", 
                                    "direction", "influence", "initiative", 
                                    "management", "power", "capacity", 
                                    "conduction", "conveyance", "directorship", 
                                    "domination", "foresight", "hegemony", 
                                    "pilotage", "preeminence", "primacy", 
                                    "superiority", "supremacy", "sway", 
                                    "superintendency"))

# Retrieve leadership words and their TF-IDF
leader_words <- inner_join(tf_idf, leadership_dict, 
                           by = c("tokens" = "words"))


write.csv(leader_words, "leader_words.csv", row.names = F)


