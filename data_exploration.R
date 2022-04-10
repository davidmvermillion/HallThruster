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
# https://githubhot.com/repo/r-lib/fs/issues/227
htdata <- map(dir_ls(path = paste0(getwd(), "/data")), read_json)

# Un-nest data
# https://www.r-bloggers.com/2018/10/converting-nested-json-to-a-tidy-data-frame-with-r/
ht2 <- enframe(unlist(htdata))

# Continue importimg, then find way to sort and look for interesting info from metadata.
