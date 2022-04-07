# Initialize workspace
library(tidyverse)
library(rjson)


# Load data
htdata <- fromJSON(file = '9780.json')

# Un-nest data
# https://www.r-bloggers.com/2018/10/converting-nested-json-to-a-tidy-data-frame-with-r/
ht2 <- enframe(unlist(htdata))

# Continue importimg, then find way to sort and look for interesting info from metadata.
