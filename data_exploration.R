# Initialize workspace ----
packages <- c('tidyverse', 'purrr', 'jsonlite', 'fs', 'tidytext', 'textdata',
              'wordcloud', 'RColorBrewer', 'wordcloud2', 'webshot',
              'htmlwidgets')
lapply(packages, library, character.only = TRUE)
webshot::install_phantomjs()

# ggplot Theme ----
theme_generic <- function(base_size = 12,
                          base_family = "",
                          base_line_size = base_size / 170,
                          base_rect_size = base_size / 170){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      axis.title.y = element_text(angle = 0, vjust = 0, hjust = 0.5, size = 15,
                                  color = "grey55", margin =
                                    margin(t = 0, r = 7, b = 0, l = 0,
                                           unit = "pt")),
      axis.title.x = element_text(hjust = 0, size = 15, color = "grey55"),
      axis.text = element_text(size = 12, color = "grey60"),
      axis.line = element_line(color = "grey60"),
      axis.ticks = element_line(color = "grey60"),
      plot.title = element_text(hjust = 0.5, size = 40, color = "grey40",
                                margin =
                                  margin(t = 0, r = 0, b = 10, l = 0,
                                         unit = "pt")),
      panel.grid = element_blank(),
      plot.subtitle = element_text(hjust = 0.5, size = 20, color = "grey40",
                                   margin =
                                     margin(t = 10, r = 0, b = 0, l = 0,
                                            unit = "pt")),
      legend.title = element_text(size = 15, color = "grey40"),
      legend.text = element_text(size = 12, color = "grey30"),
      complete = TRUE
    )
}


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

# Bar chart
cities_highlight <- topcities %>% 
  filter(Cities == "Cleveland")

topcities %>% 
  ggplot(
    aes(x = reorder(Cities, Frequency), y = Frequency)
  ) +
  geom_bar(stat = 'identity', fill = "#c2f2f7") +
  geom_bar(data = cities_highlight,
           aes(x = reorder(Cities, Frequency), y = Frequency),
           stat = "identity", fill = "#34d5e3") +
  coord_flip() +
  theme_generic() +
  ggtitle("Cleveland appeared 57 times") +
  labs(y = "Frequency",
       x = "Cities") +
  theme(plot.margin =
          margin(t = 10, r = 50, b = 10, l = 10,
                 unit = "pt"))
ggsave("top_cities.svg", device = "svg", path = "Images")
ggsave("top_cities.jpeg", device = "jpeg", path = "Images")

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

# Lead center wordcloud
set.seed(200)
wordcloud(words = leadcenter$Organization, freq = leadcenter$Frequency, min.freq = 2, 
          max.words = 200, random.order = FALSE, rot.per = 0,
          colors = brewer.pal(8, "Dark2"), scale = c(3.5, .5))

# Bar graph

leadcenter_highlight <- topleadcenters %>% 
  filter(Organization == "Glenn Research Center")

topleadcenters %>% 
  ggplot(
    aes(x = reorder(Organization, Frequency), y = Frequency)
  ) +
  geom_bar(stat = 'identity', fill = "#c2f2f7") +
  geom_bar(data = leadcenter_highlight,
           aes(x = reorder(Organization, Frequency), y = Frequency),
           stat = "identity", fill = "#34d5e3") +
  coord_flip() +
  theme_generic() +
  ggtitle("Only one NASA agency was a\ntop five lead research center") +
  labs(y = ("Frequency"),
       x = ("Org")) +
  theme(plot.margin =
          margin(t = 10, r = 10, b = 10, l = 10,
                 unit = "pt"))
# Can't save by code because of rendering issues
ggsave("leadcenters_bar.svg", device = "svg", path = "Images")
ggsave("leadcenters_bar.jpeg", device = "jpeg", path = "Images")

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

# Bar graph

supportcenter_highlight <- topsupportcenters %>% 
  filter(Organization %in% c("Glenn Research Center", 
           "Jet Propulsion Laboratory", "Marshall Space Flight Center"))

topsupportcenters %>% 
  ggplot(
    aes(x = reorder(Organization, Frequency), y = Frequency)
  ) +
  geom_bar(stat = 'identity', fill = "#c2f2f7") +
  geom_bar(data = supportcenter_highlight,
           aes(x = reorder(Organization, Frequency), y = Frequency),
           stat = "identity", fill = "#34d5e3") +
  coord_flip() +
  theme_generic() +
  ggtitle("Three NASA agencies\nwere top supporters") +
  labs(y = ("Frequency"),
       x = ("Org")) +
  theme(plot.margin =
          margin(t = 10, r = 10, b = 10, l = 10,
                 unit = "pt"))
ggsave("supportcenters_bar.svg", device = "svg", path = "Images")
ggsave("supportcenters_bar.jpeg", device = "jpeg", path = "Images")

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

# Conclusion
# Space Technology Mission Directorate was almost exclusively the responsible center.
# Only two mentions were made of the Human Exploration and Operations Mission Directorate


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

## People ----
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

## Text ----
# Title sentiment analysis ----

# Pull in titles
title <- htdata %>% 
  filter(grepl('title', name)) %>% 
  filter(grepl('libraryItems', name) == FALSE) %>% 
  filter(grepl('program', name) == FALSE) %>% 
  filter(grepl('Node', name) == FALSE) %>% 
  filter(grepl('Document', name) == FALSE) %>% 
  arrange(value)

# Rename variables
title <- as_tibble(title) %>% rename(
  Project = name,
  Title = value
)

# Tokenize titles
title_analysis <- title %>% unnest_tokens(word, Title) %>% anti_join(stop_words)
title_group <- title_analysis %>% count(word, sort = TRUE)

# Sentiment Loading
afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc") # requires citation when used

# Sentiments
title_nrc <- title_group %>% inner_join(nrc) %>%
  count(word, sort = TRUE)
title_afinn <- title_group %>% inner_join(afinn) %>%
  count(word, sort = TRUE)
title_bing <- title_group %>% inner_join(bing) %>%
  count(word, sort = TRUE)

# None of those had anything useful. Trying again with descriptions

# Look at common words
title_bar <- slice_head(title_group, n = 10)

title_highlight <- title_bar %>% 
  filter(word == "ii")

title_bar %>% 
  ggplot(
    aes(x = reorder(word, n), y = n)
  ) +
  geom_bar(stat = 'identity', fill = "#c2f2f7") +
  geom_bar(data = title_highlight,
           aes(x = reorder(word, n), y = n),
           stat = "identity", fill = "#34d5e3") +
  coord_flip() +
  theme_generic() +
  ggtitle("21 studies\nwere in Phase II") +
  labs(y = ("Word Frequency"),
       x = ("Title\nWords")) +
  theme(plot.margin =
          margin(t = 10, r = 50, b = 10, l = 10,
                 unit = "pt"))
ggsave("title_bar.svg", device = "svg", path = "Images")
ggsave("title_bar.jpeg", device = "jpeg", path = "Images")

# Wordcloud notes from https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
set.seed(200)
test <- wordcloud(words = title_group$word, freq = title_group$n, min.freq = 2, 
          max.words = 200, random.order = FALSE, rot.per = 0,
          colors = brewer.pal(8, "Dark2"), scale = c(8, .35))
# Use for sizing to export https://stackoverflow.com/questions/9245519/how-can-one-increase-size-of-plotted-area-wordclouds-in-r

# Description sentiment analysis ----

# Pull in descriptions
description <- htdata %>% 
  filter(grepl('description', name)) %>% 
  filter(grepl('Document', name) == FALSE) %>% 
  filter(grepl('file', name) == FALSE) %>% 
  filter(grepl('library', name) == FALSE) %>% 
  filter(grepl('Code', name) == FALSE)

# Remove HTML tags and URLs
description$value <- 
  str_remove_all(description$value, c("<p>" = "", "</p>" = "", "<strong>" = "",
                                      "</strong>" = "", "<li>" = "",
                                      "</li>" = "", "<ul>" = "", "</ul>" = "",
                                      "<a>" = "", "</a>" = "",
                                      "<a target=\"_blank\"" = "", "&nbsp" = "",
                                      "<.*>" = "", "href.*>" = "", ":" = ": ",
                                      ";" = " "))

# Tokenize description
description_analysis <- description %>% unnest_tokens(word, value) %>% anti_join(stop_words)
description_group <- description_analysis %>% count(word, sort = TRUE)

# Sentiments
description_nrc <- description_group %>% inner_join(nrc) %>%
  count(word, sort = TRUE)
description_afinn <- description_group %>% inner_join(afinn) %>%
  count(word, sort = TRUE)
description_bing <- description_group %>% inner_join(bing) %>%
  count(word, sort = TRUE)

# nrc has something, but it isn't particularly useful for my story. The others
# are completely useless.

# Look at common words
description_bar <- slice_head(description_group, n = 10)

# Word cloud shows a lot of admin words
set.seed(200)
wordcloud(words = description_group$word, freq = title_group$n, min.freq = 2, 
          max.words = 100, random.order = FALSE, rot.per = 0,
          colors = brewer.pal(8, "Dark2"), scale = c(8, .35))

# Bar chart

description_highlight <- description_bar %>% 
  filter(word == "phase")

description_bar %>% 
  ggplot(
    aes(x = reorder(word, n), y = n)
  ) +
  geom_bar(stat = 'identity', fill = "#c2f2f7") +
  geom_bar(data = description_highlight,
           aes(x = reorder(word, n), y = n),
           stat = "identity", fill = "#34d5e3") +
  coord_flip() +
  theme_generic() +
  ggtitle("Phase was referenced >1k times",
          subtitle = "Nearly 12 times per study") +
  labs(y = ("Word Frequency"),
       x = ("Title\nWords")) +
  theme(plot.margin =
          margin(t = 10, r = 50, b = 10, l = 10,
                 unit = "pt"),
        plot.subtitle = element_text(vjust = 2.75, color = "grey50"))
ggsave("description_bar.svg", device = "svg", path = "Images")
ggsave("description_bar.jpeg", device = "jpeg", path = "Images")

# Visuals ----

# Next steps ----
# Looking for interesting info from metadata

# Data questions:
# Where are most of these papers coming from (cities and research centers)?
# Who works the most on this topic? [leadandsupportcenters]
# What are the time distributions of starting and ending years and months and last updated dates?
# How many directors worked on more than one of these projects? (contactId)
# Sentiment analysis of titles and descriptions to find main commonalities (and word correlations)
# Similarities here: https://www.tidytextmining.com/nasa.html
# Clean up library loading section
# Add a way to download the images