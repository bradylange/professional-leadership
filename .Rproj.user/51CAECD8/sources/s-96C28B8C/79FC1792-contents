# Developer: Brady Lange
# Date: 02/23/2020
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
year_dict <- tibble(year = c('2018', '2019', '2020', '2021'))

split_job_postings <- function(jobs)
{
    # Job postings are not a tibble
    if (!is_tibble(jobs))
    {
        titles <- str_split(jobs[1], '\\\",')[[1]] %>% 
            str_remove_all('c\\(|\\(|\\)|\"|\\r|\\t|\\n|\\\\n') %>%
            str_trim() %>%
            str_to_title() %>%
            str_replace_all('\\s+', ' ')
        
        companies <- str_split(jobs[2], '\\\",')[[1]] %>%
            str_remove_all('c\\(|\\(|\\)|\"|\\r|\\t|\\n|\\\\n') %>%
            str_trim() %>%
            str_to_title() %>%
            str_replace_all('\\s+', ' ')
        
        locations <- str_split(jobs[3], '\\\",')[[1]] %>%
            str_remove_all('c\\(|\\(|\\)|\"|\\r|\\t|\\n|\\\\n') %>%
            str_trim() %>%
            str_to_title() %>%
            str_replace_all('\\s+', ' ')
        
        descriptions <- str_split(jobs[4], '\\s\\\"')[[1]] %>%
            str_remove_all('c\\(|\\(|\\)|\\\"|\\\",|\\r|\\t|\\n|\\\\n') %>%
            str_replace_all('([a-z]|[)])([A-Z])', '\\1. \\2') %>%
            str_replace_all('([a-z][.]|:)([a-z]|[A-Z])', '\\1 \\2') %>%
            str_replace_all('·', '.') %>%
            str_replace_all('\\.\\.', '.') %>%
            str_trim() %>%
            str_replace_all(',$', '.') %>%
            str_replace_all('\\s+', ' ')
        
        jobs <- tibble(title = titles, company = companies, 
                       location = locations, description = descriptions)
    }
    return(jobs)
}

clean_job_postings <- function(jobs) 
{
    titles <- jobs$title %>% 
        str_remove_all('\\||\\$|[:punct:]') %>%
        str_replace_all('\\/|\\-|\\–', ' ') %>%
        str_replace_all('&', 'and') %>%
        str_trim() %>%
        str_to_lower() %>%
        str_replace_all('\\s+', ' ')
    
    companies <- jobs$company %>%
        str_remove_all('\\||\\$|[:punct:]') %>%
        str_replace_all('\\/|\\-|\\–', ' ') %>%
        str_replace_all('&', 'and') %>%
        str_trim() %>%
        str_to_lower() %>%
        str_replace_all('\\s+', ' ')
    
    locations <- jobs$location %>%
        str_remove_all('\\||\\$|[:punct:]') %>%
        str_replace_all('\\/|\\-|\\–', ' ') %>%
        str_replace_all('&', 'and') %>%
        str_trim() %>%
        str_to_lower() %>%
        str_replace_all('\\s+', ' ')
    
    descriptions <- jobs$description %>%
        str_replace_all('&', 'and') %>%
        str_trim() %>%
        str_replace_all('\\s+', ' ')
    
    jobs <- tibble(title = titles, company = companies, 
                   location = locations, description = descriptions)
    
    return(jobs)
}

old_jobs <- tibble(title = character(), company = character(), 
                   location = character(), description = character())
all_jobs <- tibble(title = character(), company = character(), 
                   location = character(), description = character())

# Seperate job postings into old and total
for (file in files)
{
    date <- tibble(month = str_extract(files[1], month_dict$month) %>%
                       str_subset('[A-z]'), 
                   year = str_extract(files[1], year_dict$year) %>%
                       str_subset('[0-9]'))
    jobs <- readRDS(file)
    jobs <- split_job_postings(jobs)
    if (date$month %in% month_dict$month[0:7] && date$year %in% year_dict$year[2])
    {
        old_jobs <- add_row(old_jobs, title = jobs$title, company = jobs$company,
                            location = jobs$location, description = jobs$description)
    }
    all_jobs <- add_row(all_jobs, title = jobs$title, company = jobs$company,
                        location = jobs$location, description = jobs$description)
}