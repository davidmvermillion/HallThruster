# Initialize workspace
library(tidyverse)
library(purrr)
library(jsonlite)
library(fs)

# Load data ----
# https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once
# https://stackoverflow.com/questions/35421870/reading-multiple-json-files-in-a-directory-into-one-data-frame
# https://githubhot.com/repo/r-lib/fs/issues/227
htdata <- map(dir_ls(path = "data"), read_json)

# Un-nest data
# https://www.r-bloggers.com/2018/10/converting-nested-json-to-a-tidy-data-frame-with-r/
htdata <- enframe(unlist(htdata))

# Remove unnecessary characters
# https://reactgo.com/r-remove-first-n-characters-string/
htdata$name <- substring(htdata$name, 6)
htdata$name <- str_remove(htdata$name, ".json.project")

# Exploring cities ----

cities <- htdata %>% 
  filter(grepl('city', name)) %>% 
  filter(!value == "")

# https://stackoverflow.com/questions/21565605/counting-the-frequency-of-an-element-in-a-data-frame
uniquecities <- as.data.frame(table(cities$value))

# Looking for interesting info from metadata

# Data questions:
# Where are most of these papers coming from (cities and research centers)?
# Who works the most on this topic?
# What are the time distributions of starting and ending years and months and last updated dates?
# How many directors worked on more than one of these projects? (contactId)
# Sentiment analysis of titles and descriptions to find main commonalities