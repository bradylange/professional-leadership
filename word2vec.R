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
descSent <- tokenize_sentences(jobs$description, strip_punct = F) %>%
  unlist(.)
# Place job description sentences into a tibble column
descSent <- tibble(job_description_sentence = descSent)

# Save job description sentences
saveRDS(descSent, "./job_postings/jobs_sentences.RData")
write.csv(descSent, "./job_postings/jobs_sentences.csv", row.names = F)

# Load job description sentences 
descSent <- readRDS("./job_postings/jobs_sentences.RData")