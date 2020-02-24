# Developer: Brady Lange
# Date: 11/20/2019
# Description: Document clustering.

# Set-up workspace
graphics.off()
rm(list = ls())

# Load libraries
library(tidyverse)
library(proxy)
library(tm)

# https://rstudio-pubs-static.s3.amazonaws.com/266040_d2920f956b9d4bd296e6464a5ccc92a1.html

# Extract only sentence that contain both lead* and manage*
lead_desc_sent <- desc_sent %>%
    filter(
        str_detect(
            str_to_lower(sentence), 
            regex(".*(manage.*lead|lead.*manage).*")
        )
    )

# Save lead/manage sentences
saveRDS(lead_desc_sent, "./data/lead_desc_sent.RData")
write.csv(lead_desc_sent, "./data/lead_desc_sent.csv", row.names = F)

# Load lead/manage sentences
lead_desc_sent <- readRDS("./data/lead_desc_sent.RData")

# TF-IDF 
my_tf_idf <- function(vector)
{
    job_descs_corpus <- Corpus(VectorSource(vector))
    control_list <- list(removePunctuation = T, stopwords = T, tolower = T)
    tf <- TermDocumentMatrix(job_descs_corpus, control = control_list) %>%
        as.matrix()
    
    idf <- log(ncol(tf) / (1 + rowSums(tf != 0))) %>%
        diag()
    return (crossprod(tf, idf))
}

# Retrieve TF-IDF 
job_desc_tf_idf <- my_tf_idf(lead_desc_sent$sentence[1:10])

# Calculate pair-wise distance matrix 
cos_dist <- dist(job_desc_tf_idf, method = "cosine")

# Heirachical clustering 
cluster <- hclust(cos_dist, method = "ward.D")
plot(cluster)
rect.hclust(cluster, 5)

# Split into 5 clusters
groups <- cutree(cluster, 5)

# Distribution size within clusters
table(groups)

# Display cluster groupings
lead_desc_sent$sentence[1:10][groups == 2]