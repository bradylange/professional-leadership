# Developer: Brady Lange
# Date: 10/15/2019
# Description: 

# Set-up workspace
graphics.off()
rm(list = ls())

# Load libraries
library(tidyverse)
library(tidytext)

# Retrieve file names
files <- list.files(path = "./job_postings", pattern = "\\d+.RData",
                    full.names = T, recursive = F)
main_file <- list.files("./job_postings", pattern = "jobs.RData",
                        full.names = T, recursive = F)

month_dict <- tibble(month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                               "Aug", "Sep", "Oct", "Nov", "Dec"))

clean_job_postings <- function(jobs)
{
    if (!is_tibble(jobs))
    {
        titles <- str_split(jobs[1], regex(","))[[1]] %>% 
            str_remove_all(., regex("[c(]|[)]|[\"]")) %>% 
            str_trim()
        companies <- str_split(jobs[2], regex(","))[[1]] %>% 
            str_remove_all(., regex("[c(]|[)]|[\"]|[\\\\n]")) %>% 
            str_trim()
        locations <- str_split(jobs[3], regex("\\\","))[[1]] %>% 
            str_remove_all(., regex("[c(]|[)]|[\"]")) %>% 
            str_trim()
        descriptions <- str_split(test[4], regex("
                             \\n
                             ", comments = TRUE))[[1]] %>% 
            str_replace_all("([a-z]|[)])([A-Z])", "\\1. \\2") %>%
            str_replace_all("([a-z][.]|:)([a-z]|[A-Z])", "\\1 \\2") %>%
            str_replace_all(regex("\r|\t|\n"), " ") %>%
            str_replace_all("©|®|™|°|–|—|\\\"|\\\",", "") %>%
            str_replace_all("·", ".") %>%
            str_replace_all("\\.\\.", ".") %>%
            str_trim()
        jobs <- tibble(title = titles, company = companies, location = locations,
                       description = descriptions)
    }
    return(jobs)
}

t <- readRDS(files[1])
hi <- clean_job_postings(t)


old_jobs <- tibble(title = character(), company = character(), 
                   location = character(), description = character())
all_jobs <- tibble(title = character(), company = character(), 
                   location = character(), description = character())
for (file in files)
{
    date <- tibble(month = str_extract(file, month_dict$month)) %>%
        filter(!is.na(.))
    jobs <- readRDS(file)
    jobs <- clean_job_postings(jobs)
    if (date %in% filter(month_dict, month %in% month[0:7]))
    {
        old_jobs <- rbind(old_jobs, jobs)
    }
    all_jobs <- add_row(all_jobs, jobs)
}






















test <- readRDS(files[1])

str_split(test[1], regex(","))[[1]] %>% 
    str_remove_all(., regex("^[c(]$|[)]|[\"]")) %>% 
    str_trim() %>%
    head(.)
str_split(test[2], regex(","))[[1]] %>% 
    str_remove_all(., regex("^[c(]$|[)]|[\"]|^[\\\\n]$")) %>% 
    str_trim() %>%
    head(.)
str_split(test[3], regex("\\\","))[[1]] %>% 
    str_remove_all(., regex("^[c(]$|[)]|[\"]")) %>% 
    str_trim() %>%
    tail(.)
str_split(test[4], regex("\\\","))[[1]] %>% 
    str_replace_all(., regex("^[c(]$|[)]|[\"]|^[\\\\n]$"), " ") %>% 
    str_trim() %>%
    head(.)


str_split(test[4], regex("
                         \\n
                         ", comments = TRUE))[[1]] %>% 
    str_replace_all("([a-z]|[)])([A-Z])", "\\1. \\2") %>%
    str_replace_all("([a-z][.]|:)([a-z]|[A-Z])", "\\1 \\2") %>%
    str_replace_all(regex("\r|\t|\n"), " ") %>%
    str_replace_all("©|®|™|°|–|—|\\\"|\\\",", "") %>%
    str_replace_all("·", ".") %>%
    str_replace_all("\\.\\.", ".") %>%
    str_trim() %>%
    head(.)

# \", \n\"
cat("\\\",")

