# Initialize workspace
library(tidyverse)
library(purrr)
library(jsonlite)
library(fs)

# Load data
# https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once
# https://stackoverflow.com/questions/35421870/reading-multiple-json-files-in-a-directory-into-one-data-frame
# https://githubhot.com/repo/r-lib/fs/issues/227
htdata <- map(dir_ls(path = "data"), read_json)

# Un-nest data
# https://www.r-bloggers.com/2018/10/converting-nested-json-to-a-tidy-data-frame-with-r/
ht2 <- enframe(unlist(htdata))

# Remove path
# https://reactgo.com/r-remove-first-n-characters-string/
ht2$name <- substring(ht2$name, 6)

# Still need to find a way to filter the names to something useful

# Continue importing, then find way to sort and look for interesting info from metadata.
