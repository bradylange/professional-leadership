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
jobWords <- jobs %>%
  unnest_tokens(token, description) %>%
  count(title, token, sort = T)

# Check if correct
dim(unique(jobWords$token)) == dim(jobWords)
jobWords %>% 
  group_by(title, token) %>%
  unique()

# Total the amount of tokens per job title
totalWords <- jobWords %>%
  group_by(title) %>%
  summarize(total = sum(n)) %>%
  arrange(desc(total))

# Combine specific tokens per job title and total tokens per job title
jobwords <- left_join(jobwords, totalWords)

# TF-IDF 
tfidf <- jobWords %>% 
  bind_tf_idf(token, title, n)

# Leadership terms - reference (https://www.thesaurus.com/browse/leadership)
leadershipDict <- tibble(word = c("leadership", "leader", "lead", 
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
leaderWords <- inner_join(tfidf, leadershipDict, 
                          by = c("token" = "word"))


write.csv(leaderWords, "leader_words.csv", row.names = F)