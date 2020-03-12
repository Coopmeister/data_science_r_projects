# Import main libraries

# To get basic R packages like dplyr
library("tidyverse")

#To work with dates
library("lubridate")

# To allow R to interact with Python and its associated modules
library("reticulate")

# Get the latest coronavirus data (~ 5:45 CST 3/12/20)
devtools::install_github("RamiKrispin/coronavirus", force = TRUE)
library("coronavirus")

# Use Python and modules already installed in Anaconda
use_condaenv()

# Import Python modules
pd = import("pandas")
np = import("numpy")
plt = import("matplotlib.pyplot")
sns = import("seaborn")

covid <- coronavirus

# Check out the data
covid %>% glimpse()

# Wrangle data to look at running total cases in U.s. by type since March 1
covid_us <- covid %>%
   rename(country = Country.Region) %>%
   filter(country == "US",) %>%
   select(date, type, cases) %>%
   arrange(date) %>%
   group_by(date, type) %>%
   summarise(total_cases = sum(cases)) %>%
   ungroup() %>%
   group_by(type) %>%
   mutate(running_total = cumsum(total_cases)) %>%
   ungroup() %>%
   select(date, type, running_total) %>%
   filter(date %>% between(ymd("2020-03-01"), right = ymd("2020-03-13")))




g = sns$barplot(x = "date", y = "running_total", hue = "type", data = covid_us)

plt$show(g)
