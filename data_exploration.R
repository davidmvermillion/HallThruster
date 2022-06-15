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

# Top five lead centers
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

# Top five support centers
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

# Top five responsible centers
topresponsiblecenters <- slice_head(responsiblecenter, n = 5)

# Centers in both roles ----
leadandsupport <- inner_join(topleadcenters, topsupportcenters, by = "Organization")

# Rename variables
leadandsupport <- as_tibble(leadandsupport) %>% rename(
  Lead = Frequency.x,
  Support = Frequency.y
)
leadandsupport$Total <- leadandsupport$Lead + leadandsupport$Support

# Sort by frequency
leadandsupport <- arrange(leadandsupport, desc(Total))

## Timing ----

# Starting years ----
StartYear <- htdata %>% 
  filter(grepl('startYear', name)) %>% 
  arrange(value)

StartYear <- as.data.frame(table(StartYear$value))

# Rename variables
StartYear <- as_tibble(StartYear) %>% rename(
  Year = Var1,
  Frequency = Freq
)

# Sort by frequency
StartYear <- arrange(StartYear, desc(Frequency))

# Top five starting years
topstartyear <- slice_head(StartYear, n = 5)

# Starting months ----
StartMonth <- htdata %>% 
  filter(grepl('startMonth', name)) %>% 
  arrange(value)

StartMonth <- as.data.frame(table(StartMonth$value))

# Rename variables
StartMonth <- as_tibble(StartMonth) %>% rename(
  Month = Var1,
  Frequency = Freq
)

# Sort by frequency
StartMonth <- arrange(StartMonth, desc(Frequency))

# Top five starting months
topstartmonth <- slice_head(StartMonth, n = 5)

# Ending years ----
EndYear <- htdata %>% 
  filter(grepl('endYear', name)) %>% 
  arrange(value)

EndYear <- as.data.frame(table(EndYear$value))

# Rename variables
EndYear <- as_tibble(EndYear) %>% rename(
  Year = Var1,
  Frequency = Freq
)

# Sort by frequency
EndYear <- arrange(EndYear, desc(Frequency))

# Top five ending years
topendyear <- slice_head(EndYear, n = 5)

# Ending months ----
EndMonth <- htdata %>% 
  filter(grepl('endMonth', name)) %>% 
  arrange(value)

EndMonth <- as.data.frame(table(EndMonth$value))

# Rename variables
EndMonth <- as_tibble(EndMonth) %>% rename(
  Month = Var1,
  Frequency = Freq
)

# Sort by frequency
EndMonth <- arrange(EndMonth, desc(Frequency))

# Top five ending months
topendmonth <- slice_head(EndMonth, n = 5)

# Directors ----
Directors <- htdata %>% 
  filter(grepl('programDirectors.contactId', name)) %>% 
  arrange(value)

Directors <- as.data.frame(table(Directors$value))

# Rename variables
Directors <- as_tibble(Directors) %>% rename(
  ContactId = Var1,
  Frequency = Freq
)

# Sort by frequency
Directors <- arrange(Directors, desc(Frequency))

# Add names
# I did this manually because I wasn't able to properly reference the names automatically
names <- c('Jason L Kessler', 'Claudia M Meyer', 'Mary J Werkheiser',
           'Michael R Lapointe', 'Christopher E Baker', 'Trudy F Kortes',
           'Christopher L Moore')
Directors$names <- names

# Top five directors
topdirectors <- slice_head(Directors, n = 5)

# 7 total program directors. One highly prolific program director

# Principal Investigators ----
# Contact ID
PI <- htdata %>% 
  filter(grepl('principalInvestigators.contactId', name))%>% 
  arrange(value)
PI$name <- str_remove(PI$name, ".principalInvestigators.contactId")

# Contact full name
PI2 <- htdata %>% 
  filter(grepl('principalInvestigators.fullName', name)) %>% 
  filter(grepl('Inverted', name) == FALSE) %>% 
  arrange(value)
PI2$name <- str_remove(PI2$name, ".principalInvestigators.fullName")

PIid <- inner_join(PI, PI2, by = "name") %>% select(-name)

# Rename variables
PIid <- as_tibble(PIid) %>% rename(
  contactId = value.x,
  name = value.y
)

# Sort by ID
PI2 <- as.data.frame(table(PIid$contactId))
# Rename variables
PI2 <- as_tibble(PI2) %>% rename(
  contactId = Var1,
  Frequency = Freq
)
# Sort by frequency
PI2 <- arrange(PI2, desc(Frequency))

# Sort by Name
PI3 <- as.data.frame(table(PIid$name))
# Rename variables
PI3 <- as_tibble(PI3) %>% rename(
  name = Var1,
  Frequency = Freq
)
# Sort by frequency
PI3 <- arrange(PI3, desc(Frequency))

# Top five principle investigators
topPIid <- slice_head(PI2, n = 5)
topPIname <- slice_head(PI3, n = 5)
topPI <- topPIname
topPI$contactId <- topPIid$contactId

# Clean section
rm(list = c("topPIid", "topPIname"))

# Title sentiment analysis ----

# Description sentiment analysis ----

# Visuals ----

# Next steps ----
# Looking for interesting info from metadata

# Data questions:
# Where are most of these papers coming from (cities and research centers)?
# Who works the most on this topic? [leadandsupportcenters]
# What are the time distributions of starting and ending years and months and last updated dates?
# How many directors worked on more than one of these projects? (contactId)
# Sentiment analysis of titles and descriptions to find main commonalities