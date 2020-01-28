#################################################################################
#                        #dailycoding 26:  Kobe Bryant Dedication               #
#                           # Average points / season                           #
#################################################################################


# 1.0  Import Libraries
library("tidyverse")
# install.packages("ballr")
library("ballr")
# install.packages("esquisse")
library("esquisse")
library("magrittr")

# 2.0 Use NBAPlayerPerGameStats from ballr package to pull in Kobe's stats
data <- NBAPlayerPerGameStats("/players/b/bryanko01.html")

# 3.0 Isolate the features and rows needed for analysis
kobe <- data %>%
    select(season, pts) %>%
    slice(1:20)
  
# 4.0 Call esquisser() function from esquisse to bring up interactive graph builder
esquisse::esquisser(kobe)
  

# Here is the code that the esquisse package produces
library(ggplot2)

ggplot(kobe2) +
 aes(x = season, weight = pts) +
 geom_bar(fill = "#7301a8") +
 labs(x = "Season", y = "Average Points", title = "Kobe Bryant - LA Lakers Legend", subtitle = "Average Points / Season", caption = "Kobe's Overall Career Average was 25.0 Points") +
 theme_minimal()

