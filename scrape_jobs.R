# Developer: Brady Lange
# Date: 04/24/2019
# Description: Scrapes Indeed.com's job postings from the last day.

# Set-up workspace
graphics.off
rm(list = ls())

# Load libraries
library(tidyverse)
library(stringi)
library(rvest)
library(tools)

# =============================================================================
# Scrape Job Applications
# =============================================================================
# -----------------------------------------------------------------------------
# Concatenate Uniform Resource Locater Method
# -----------------------------------------------------------------------------
# Combines a given URL and page start number
concatenateURL <- function(pageStartNum, url = firstPageURL) 
{
  # Combine URL and page start number 
  str_c(url, pageStartNum)
}

# -----------------------------------------------------------------------------
# Concatenate Indeed Method
# -----------------------------------------------------------------------------
# Combines Indeed's domain and URL path
concatenateIndeed <- function(urlPath) 
{
  # Combine Indeed's URL and path
  str_c("https://indeed.com", urlPath)
}

# -----------------------------------------------------------------------------
# Get Uniform Resource Locaters Method
# -----------------------------------------------------------------------------
# Retrieves the navigation page URL's specified from the URL
getPageURLs <- function(url)
{
  # Retrieve navigation page URLs
  read_html(url) %>% 
    html_nodes(".pagination a") %>%
    html_attr("href") %>%
    str_extract(., "&start=\\d+") %>%
    .[-length(.)] %>%
    lapply(., concatenate_url) %>% 
    unlist(.)
}

# -----------------------------------------------------------------------------
# Get Titles Method
# -----------------------------------------------------------------------------
# Retrieves the job titles on the specified page
getTitles <- function(page)
{
  # Retrieve job titles from page
  page %>%
    html_nodes(xpath = "//a[@data-tn-element='jobTitle']") %>%
    html_attr("title") %>%
    toTitleCase()
}

# -----------------------------------------------------------------------------
# Get Companies Method
# -----------------------------------------------------------------------------
# Retrieves the job companies on the specified page 
getCompanies <- function(page)
{
  # Retrieve job companies from page 
  page %>%
    html_nodes(xpath = "//*[@class='company']") %>%
    html_text() %>%
    toTitleCase() %>%
    str_trim()
}

# -----------------------------------------------------------------------------
# Get Locations Method
# -----------------------------------------------------------------------------
# Retrieves the job locations on the specified page
getLocations <- function(page)
{
  # Retrieve job locations from page
  page %>%
    html_nodes(xpath = "//*[@class='location']") %>%
    html_text() %>%
    toTitleCase() %>%
    str_trim()
}

# -----------------------------------------------------------------------------
# Get Card Links Method
# -----------------------------------------------------------------------------
# Retrieves the job card links on the specified page
getCardLinks <- function(page)
{
  # Retrieve job card links from page 
  page %>% 
    html_nodes(xpath = "//a[@data-tn-element='jobTitle']") %>%
    html_attr("href") %>%
    lapply(., concatenate_indeed) %>%
    unlist(.)
}

# -----------------------------------------------------------------------------
# Get Card Method
# -----------------------------------------------------------------------------
# Retrieves the job card's HTML page from the specified link
getCard <- function(link)
{
  # Link is an advertisement link
  if (!is.na(str_extract(link, "pagead")))
  {
    # Open link and retrieve correct URL
    link <- html_session(link) %>%
      .$url
  }
  # Try retrieve card's HTML page and return NA on error
  tryCatch(
    {
      # Retrieve card's HTML page
      read_html(link) %>%
        html_node(xpath = str_c("//*[@class='jobsearch-JobComponent-",
                                "description icl-u-xs-mt--md']"))
    },
    # On error return NA for bad HTML page
    error = function(e) 
    {
      NA
    }
  )
}

# -----------------------------------------------------------------------------
# Get Description Method
# -----------------------------------------------------------------------------
# Retrieves the job description from the specified job card
getDescription <- function(card)
{
  # Card page is not NA 
  if (!is.na(card))
  {
    # Retrieve card's job description and form sentences 
    description <- html_text(card) %>%
      str_replace_all("([a-z]|[)])([A-Z])", "\\1. \\2") %>%
      str_replace_all("([a-z][.]|:)([a-z]|[A-Z])", "\\1 \\2") %>%
      str_replace_all("\r|\t|\n", " ") %>%
      str_replace_all("©|®|™|°|–|—", "") %>%
      str_replace_all("·", ".") %>%
      str_replace_all("\\.\\.", ".") %>%
      str_trim() 
    # Job description text is in English, return the description text
    if (stri_enc_isascii(description) == TRUE)
    {
      description
    }
    # Job description text is not in English, return NA 
    else
    {
      NA
    }
  }
  # Card page is NA, return NA
  else 
  {
    NA
  }
}

# URL for website to be scraped - Query: Leadership terms, filter by 
# Full-Time Job, display 50, and sort by date
firstPageURL <- str_c("https://www.indeed.com/jobs?as_and&as_phr",
                      "&as_any=leadership%20leader%20lead%20supervise%20",
                      "supervisor%20manager%20manage%20administration%20",
                      "administrator%20authority%20control%20direction%20",
                      "influence%20initiative%20management%20power%20",
                      "capacity%20conduction%20conveyance%20directorship%20",
                      "domination%20foresight%20hegemony%20pilotage%20",
                      "preeminence%20primacy%20superiority%20supremacy%20",
                      "sway%20superintendency&as_not=Now%20hiring%20at%20",
                      "hiring%20event&as_ttl&as_cmp&jt=fulltime&st&as_src",
                      "&salary&radius=25&l&fromage=1&limit=50&sort=date",
                      "&psf=advsrch&vjk=7c43b6b0e1a8fa43")

# -----------------------------------------------------------------------------
# Scrape Jobs Method
# -----------------------------------------------------------------------------
# Scrapes job postings for their titles, companies, locations, and descriptions
scrapeJobs <- function(firstPageURL)
{
  # Start process time 
  processTime <<- proc.time()
  # Retrieve the date and time of the jobs being scraped 
  scrapeTime <- date()
  # Format the date and time of the jobs being scraped 
  scrapeTime <- str_replace_all(string = scrapeTime,
                                pattern = " ",
                                replacement = "_") %>%
      str_replace_all(pattern = ":", replacement = "-")

  # Retrieve the page URLs 
  pageURLs <- getPageURLs(firstPageURL)
  # Retrieve the HTML pages of all of the pages 
  pages <- lapply(pageURLs, read_html)
  
  # Retrieve all of the job titles 
  titles <- lapply(pages, getTitles) %>%
    unlist(.)
  # Retrieve all of the companies 
  companies <- lapply(pages, getCompanies) %>%
    unlist(.)
  # Retrieve all of the job locations
  locations <- lapply(pages, getLocations) %>%
    unlist(.)
  # Retrieve all of the job card links
  cardLinks <- lapply(pages, getCardLinks) %>%
    unlist(.)
  # Retrieve all of the HTML pages of all of the job cards
  cards <- lapply(cardLinks, getCard) 
  # Retrieve all of the job descriptions 
  descriptions <- lapply(cards, getDescription) %>%
    unlist(.)
  
  # Combine all of the job vector data into a tibble with unique data
  jobs <- tibble(title = titles, company = companies, 
                 location = locations, description = descriptions) %>%
    unique(.) %>%
    na.omit(.)
  # Load full data set of jobs 
  completeJobs <- readRDS("./job_postings/jobs.RData")
  # Pause the program to let data load
  Sys.sleep(0.25)
  # Combine the unique last day of jobs and the complete job data set
  completeJobs <- rbind(completeJobs, jobs) %>%
    unique(.) %>%
    na.omit(.)
  # Save the last day of jobs and complete jobs data sets
  saveRDS(jobs, file = str_c("./job_postings/", scrapeTime, ".RData"))
  saveRDS(completeJobs, file = "./job_postings/jobs.RData")
  write.csv(completeJobs, file = "./job_postings/jobs.csv", row.names = F)
  # End the process time and calculate difference
  processTime <- proc.time() - processTime
  # Convert elapsed process time from seconds to minutes 
  processTime[3] <- round(processTime[3] / 60, 2)
  # Load process time and average process time in minutes
  load("./data/process_time.RData")
  # Pause the program to let data load
  Sys.sleep(0.05)
  # Combine current process time and all process times 
  allProcTime <- rbind(allProcTime, processTime)
  # Calculate average process time in minutes
  timeAvg <- mean(allProcTime$time)
  # Save all of the process times and process time average
  save(allProcTime, timeAvg, file = "./data/process_time.RData")
  cat(str_c("Process time in minutes: ", processTime[3], "\n",
            "Average process time in minutes: ", timeAvg))
  return(jobs)
}

# Scrape Indeed.com for the last day of job postings
oneDayJobs <- scrapeJobs(firstPageURL)

# -----------------------------------------------------------------------------
# Scrape Jobs Limit Method
# -----------------------------------------------------------------------------
# Scrapes job postings for their titles, companies, locations, and descriptions
# based on a specific page start, page end, and jobs per page 
scrapeJobsLimit <- function(url, pgStart, pgEnd, jobsPerPage)
{
  scrapeTime <- date()
  scrapeTime <- str_replace_all(string = scrapeTime,
                                pattern = " ",
                                replacement = "_") %>%
    str_replace_all(pattern = ":", replacement = "-")
  # URL for website to be scraped - Query: All, filter by Full-Time Job, 
  # and sort by date
  firstPageURL <- url
  pgResults <- seq(pgStart, pgEnd, by = jobsPerPage)
  ttlJobs <- data.frame()
  for(i in seq_along(pgResults)) 
  {
    url <- str_c(firstPageURL, "&start=", pgResults[i])
    # tryCatch(
    # {
      page <- read_html(url)
      Sys.sleep(1)
    # },
    # error = function(e)
    # {
    #   page <- read_html(url)
    #   Sys.sleep(3)
    # })
    
    
    # Sys.sleep pauses R for two seconds before it resumes
    # Putting it there avoids error messages such as 
    # "Error in open.connection(con, "rb") : Timeout was reached"
    #Sys.sleep(4)
    
    # Title
    jobTitle <- page %>%
      html_nodes(xpath = "//a[@data-tn-element='jobTitle']") %>%
      html_attr("title") %>%
      toTitleCase()
    
    # Company
    jobCompany <- page %>%
      html_nodes(xpath = "//*[@class='company']") %>%
      html_text() %>%
      toTitleCase() %>%
      str_trim()
    
    # Location
    jobLocation <- page %>%
      html_nodes(xpath = "//*[@class='location']") %>%
      html_text() %>%
      toTitleCase() %>%
      str_trim()
    
    # Links
    jobLinks <- page %>% 
      html_nodes(xpath = "//a[@data-tn-element='jobTitle']") %>%
      rvest::html_attr("href")
    
    # Description
    jobDescription <- c()
    for(i in seq_along(jobLinks)) {
      
      url <- str_c("https://indeed.com/", jobLinks[i])
      # tryCatch(
      # {
        page <- read_html(url)
      # },
      # error = function(e)
      # {
      #   page <- read_html(url)
      #   Sys.sleep(3)
      # })
      # Sys.sleep pauses R for two seconds before it resumes
      # Putting it there avoids error messages such as 
      # "Error in open.connection(con, "rb") : Timeout was reached"
      #Sys.sleep(2)
      
      jobDescription[i] <- page %>%
        html_node(xpath = str_c("//*[@class='jobsearch-JobComponent-",
                                "description icl-u-xs-mt--md']")) %>%
        html_text() %>%
        str_replace_all("([a-z]|[)])([A-Z])", "\\1. \\2") %>%
        str_replace_all("([a-z][.])([a-z])", "\\1 \\2") %>%
        str_replace_all("\r|\t|\n", " ") %>% 
        str_trim()
    }
    pgJobs <- tibble(jobTitle, jobCompany, jobLocation, jobDescription)
    ttlJobs <- rbind(ttlJobs, pgJobs)
  }
  jobs <- readRDS("./job_postings/jobs.RData")
  Sys.sleep(0.25)
  jobs <- rbind(jobs, ttlJobs) %>% 
    unique(.)
  saveRDS(ttlJobs, file = str_c("./job_postings/", scrapeTime, ".RData"))
  saveRDS(jobs, file = "./job_postings/jobs.RData")
  return(ttlJobs)
}

# Scrape Indeed.com for the last day of job postings
oneDayJobs <- scrapeJobsLimit(url, 0, 990, 10)

# Retrieve file names
files <- list.files(path = "./job_postings", pattern = "\\d+.RData", 
                    full.names = T, recursive = F)
mainFile <- list.files("./job_postings", pattern = "jobs.RData", 
                       full.names = T, recursive = F)

for (file in files)
{
  f <- readRDS(file)
  f$description <-str_replace_all(f$description, "([a-z]|[)])([A-Z])", "\\1. \\2") %>%
    str_replace_all("([a-z][.]|:)([a-z]|[A-Z])", "\\1 \\2") %>%
    str_replace_all("\r|\t|\n", " ") %>%
    str_replace_all("©|®|™|°|–|—", "") %>%
    str_replace_all("·", ".") %>%
    str_replace_all("\\.\\.", ".") %>%
    str_trim() 
  saveRDS(f, "./data/test")
}

for (file in files)
{
  f <- readRDS(file)
  tryCatch(
  {
    f <- with(f, tibble(title = jobTitle, company = jobCompany, 
                        location = jobLocation, description = jobDescription)) 
  },
  error = function(e)
  {
    f <- with(f, tibble(title = title, company = company, location = location, 
                        description = description))
  })
  saveRDS(f, file)
}


f <- readRDS(files[1]) %>% as.tibble()
f$description <-str_replace_all(f$description, "([a-z]|[)])([A-Z])", "\\1. \\2") %>%
  str_replace_all("([a-z][.]|:)([a-z]|[A-Z])", "\\1 \\2") %>%
  str_replace_all("\r|\t|\n", " ") %>%
  str_replace_all("©|®|™|°|–|—", "") %>%
  str_replace_all("·", ".") %>%
  str_replace_all("\\.\\.", ".") %>%
  str_trim() 


# Load all jobs
jobs <- readRDS("./job_postings/jobs.RData")

# =============================================================================
# Server Web Scrape - RSelenium
# =============================================================================
# -----------------------------------------------------------------------------
# Scrape Descriptions Method
# -----------------------------------------------------------------------------
# Function that scrapes Indeed.com's job application descriptions and 
# returns unique words from each job description
scrapeDesc <- function(url)
{
  # On error close server connection and display error
  tryCatch(
  {
      # Error check
      as.character(url)
      
      tryCatch(
      {
          # Load Selenium 
          library(RSelenium)
          # Load tidyverse
          library(tidyverse)
          # Load tidytext
          library(tidytext)
      }, 
      error = function(e) 
      {
          # Install/Update packages
          install.packages("RSelenium")
          install.packages("tidyverse")
          install.packages("tidytext")
          install.packages("stringr")
      })
      
      # Connect to Chrome browser
      chromeDriver <<- rsDriver(browser = "chrome", version = "latest", 
                                chromever = "73.0.3683.68") 
      client <- chromeDriver$client
      client$maxWindowSize(winHand = "current")
      # Allow time to connect to server (0.50 seconds)
      Sys.sleep(0.50)
      # Set wait time for the driver to search for elements (5 seconds)
      client$setImplicitWaitTimeout(milliseconds = 10000)
      # Set wait time to time-out on page-load (30 seconds)
      client$setTimeout(type = "page load", milliseconds = 30000)
      
      print("Loading web page...")
      
      # Instantiate variable for the returned text 
      jobs <- NULL
      badJS <- 0
      # Instantiate tracker variable
      i <- 1
      # Instantiate total amount of job applications
      totalCards <- 0
      # Navigate to URL to be web scraped 
      client$navigate(url)
      # Let page load
      Sys.sleep(0.25)
      # Retrieve the application cards on the webpage in element form
      appCardElem <- client$findElements(using = "css selector", ".clickcard")
      numCardElem <<- length(appCardElem)
      print(str_c("Scraping ", url, " for job application descriptions..."))
      # Scrape applications description text
      while (TRUE)
      {
        # End of page, go to next page
        if (i == numCardElem + 1)
        {
          # Don't count poor JavaScript job cards
          totalCards <- totalCards - badJS
          # Reset tracker variable
          i <- 1
          # Retrieve the application cards on the webpage in element form
          nextPageElem <- client$findElements(using = "css selector", "b+ a .pn")
          # Click next page
          nextPageElem[[i]]$clickElement()
          # Refresh to eliminate pop-ups
          client$refresh()
          # Update element references
          appCardElem <- client$findElements(using = "css selector", ".clickcard")
          # Number of applications on current page 
          numCardElem <- length(appCardElem)
          # Total applications scraped 
          totalCards <<- totalCards + numCardElem
          # Reset poor JavaScript cards count
          badJS <- 0
        }
        # Click application card
        appCardElem[[i]]$clickElement()
        # Find the element that is located in the dynamic webpage, wrapped in 
        # try block to avoid pages that don't contain selector
        tryCatch(
        {
            suppressMessages(
            {
                descElem <- client$findElement(using = "css selector", 
                                               "#info-link-row .date , #vjs-desc , 
                                               #vjs-header")
                date <- client$findElement(using = "css selector", ".date")
                # One month of jobs postings have been scraped
                if (date$getElementText()[[1]] == "32 days ago")
                {
                  break
                }
                # Retrieve the elements text, split it by next line character, 
                # trim white space, and remove specific non-word characters 
                # Unnest the tokens by white space to prevent needed non-word 
                # characters from being removed such as (c#, c++, etc.)
                # Keep only unique tokens to avoid inaccurate analysis for each 
                # job application 
                text <- tibble(text = desc_elem$getElementText()[[1]] %>% 
                                   str_split("\n", simplify = T) %>% 
                                   str_trim(.))
                jobs[i] <- text
              })
          },
          error = function(e)
          {
            #client$dismissAlert
            descElem <- ""
            badJS <- badJS + 1
          })
        i <- i + 1
      }
      # Stop the Selenium server
      chromeDriver[["server"]]$stop() 
      
      # Print all of the applications description text
      print("Finished web scraping successfully!")
      jobs
    }, 
    # Error data 
    error = function(e) 
    {
      # Stop the Selenium server
      chromeDriver[["server"]]$stop()
      print("An error has occurred. Closing the server connection.")
      # Print error information
      print(e)
    })
}

# Retrieve all of the job postings from the last day
jobPostings <- scrapeDesc(url)