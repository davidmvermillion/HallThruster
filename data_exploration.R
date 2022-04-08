# Initialize workspace
library(tidyverse)
library(rjson)
library(data.table)
library(rio)


# Load data
# https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once
https://stackoverflow.com/questions/35421870/reading-multiple-json-files-in-a-directory-into-one-data-frame
htdata <- fromJSON(file = 'data/9780.json')
htdata2 <- 
  list.files(path="data", pattern = "*.json", full.names = T) %>% 
  map_df(~fromJSON(.))
htdata3 <- import_list(dir(path = "data", pattern = ".json"), rbind = TRUE)
htdata4 <- import(dir(path = "data", pattern = ".json"))

# Un-nest data
# https://www.r-bloggers.com/2018/10/converting-nested-json-to-a-tidy-data-frame-with-r/
ht2 <- enframe(unlist(htdata))

# Continue importimg, then find way to sort and look for interesting info from metadata.
