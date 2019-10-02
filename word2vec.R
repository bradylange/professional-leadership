# Developer: Brady Lange
# Date: 04/18/2019
# Description: Prepares job descriptions for use with Word2Vec machine 
# learning.

# Set-up workspace
graphics.off()
rm(list = ls())

# Load libraries
library(tidyverse)
library(tokenizers)

# =============================================================================
# Word2Vec
# =============================================================================
# Convert job posting descriptions into sentences and make it a vector
desc_sent <- tokenize_sentences(jobs$description, strip_punct = F) %>%
  unlist(.)
# Place job description sentences into a tibble column
desc_sent <- tibble(sentence = desc_sent)

# Save job description sentences
saveRDS(desc_sent, "./job_postings/jobs_sentences.RData")
write.csv(desc_sent, "./job_postings/jobs_sentences.csv", row.names = F)

# Load job description sentences 
desc_sent <- readRDS("./job_postings/jobs_sentences.RData")