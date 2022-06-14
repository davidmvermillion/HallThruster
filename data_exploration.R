# Initialize workspace ----
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
uniquecities <- as_tibble(uniquecities) %>% rename(
  Cities = Var1,
  Frequency = Freq
)

# Sort by frequency
uniquecities <- arrange(uniquecities, desc(Frequency))

# Top five cities
topcities <- slice_head(uniquecities, n = 5)


## Research Centers ----

# Pulling out organizations (research centers)
rcenters <- htdata %>% 
  filter(grepl('organizationName', name))

# Analyzing the lead centers ----
leadcenter <- rcenters %>% 
  filter(grepl('lead', name)) %>% 
  arrange(value)

leadcenter <- as.data.frame(table(leadcenter$value))

# Rename variables
leadcenter <- as_tibble(leadcenter) %>% rename(
  Organization = Var1,
  Frequency = Freq
)

# Sort by frequency
leadcenter <- arrange(leadcenter, desc(Frequency))

# Top five centers
topleadcenters <- slice_head(leadcenter, n = 5)

# Analyzing the support centers ----
supportcenter <- rcenters %>% 
  filter(grepl('support', name)) %>% 
  arrange(value)

supportcenter <- as.data.frame(table(supportcenter$value))

# Rename variables
supportcenter <- as_tibble(supportcenter) %>% rename(
  Organization = Var1,
  Frequency = Freq
)

# Sort by frequency
supportcenter <- arrange(supportcenter, desc(Frequency))

# Top five centers
topsupportcenters <- slice_head(supportcenter, n = 5)

# Analyzing the responsible centers ----
responsiblecenter <- rcenters %>% 
  filter(grepl('responsible', name)) %>% 
  arrange(value)

responsiblecenter <- as.data.frame(table(responsiblecenter$value))

# Rename variables
responsiblecenter <- as_tibble(responsiblecenter) %>% rename(
  Organization = Var1,
  Frequency = Freq
)

# Sort by frequency
responsiblecenter <- arrange(responsiblecenter, desc(Frequency))

# Top five centers
topresponsiblecenters <- slice_head(responsiblecenter, n = 5)

# Next steps ----
# Looking for interesting info from metadata

# Data questions:
# Where are most of these papers coming from (cities and research centers)?
# Who works the most on this topic?
# What are the time distributions of starting and ending years and months and last updated dates?
# How many directors worked on more than one of these projects? (contactId)
# Sentiment analysis of titles and descriptions to find main commonalities