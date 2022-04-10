# Initialize workspace
library(tidyverse)
library(rjson)
library(data.table)
library(rio)
library(purrr)
library(jsonlite)
library(fs)


# Load data
# https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once
# https://stackoverflow.com/questions/35421870/reading-multiple-json-files-in-a-directory-into-one-data-frame
htdata <- fromJSON(file = 'data/9780.json')
htdata2 <- 
  list.files(path="data", pattern = "*.json", full.names = T) %>% 
  map_df(~fromJSON(.))
htdata3 <- import_list(dir(path = "data", pattern = ".json"), rbind = TRUE)
htdata4 <- import(dir(path = "data", pattern = ".json"))
path <- "data"
files <- dir(path, pattern = "*.json")

data <- files %>% 
  map_df(~fromJSON(file.path(path, .), flatten = TRUE))

data <- dir(path, pattern = "*.json") %>% 
  map_df(~fromJSON(file.path(path, .), flatten = TRUE))

# https://githubhot.com/repo/r-lib/fs/issues/227
htdata5 <- map(dir_ls(path = paste0(getwd(), "/data")), read_json)

# Un-nest data
# https://www.r-bloggers.com/2018/10/converting-nested-json-to-a-tidy-data-frame-with-r/
ht2 <- enframe(unlist(htdata))

# Continue importimg, then find way to sort and look for interesting info from metadata.
