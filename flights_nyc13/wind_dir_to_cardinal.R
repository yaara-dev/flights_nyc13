library(rvest)
library(tidyverse)


#### convert wind direction from degrees to 16 compass directions ####
url <-
  'http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm'
page <- read_html(url)
directions_raw <-
  page %>% html_node('td table') %>% html_table(header = TRUE)

directions <- directions_raw %>%
  set_names(~ tolower(sub(' Direction', '', .x))) %>%
  slice(-1) %>%
  separate(degree,
           c('degree_min', 'degree_max'),
           sep = '\\s+-\\s+',
           convert = TRUE)
write.csv(directions, 'wind_directions.csv', row.names = FALSE)


#####start code from here####
directions <- read.csv('wind_directions.csv')

flights_full <- flights_full %>%
  mutate(wd_cardinal = cut(
    as.numeric(wind_dir),
    breaks = c(0, directions$degree_max, 360),
    labels = c(directions$cardinal, 'N')
  ))
#note: when the wind direction is 0 degrees the wd_cardinal is NA.
#Also, the wind speed is 0.
#This is correct because if the wind is not moving then it does not have a direction.

