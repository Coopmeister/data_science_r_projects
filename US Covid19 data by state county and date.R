library(tidyverse)
library(readxl)
library(readr)
library(writexl)
library(lubridate)

options(max.print=.Machine$integer.max)


# Source:  https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/   # Updated daily. 

confirmed       <- readr::read_csv("C:\\Personal Data Science Projects\\data\\covid_confirmed_usafacts3.csv")
deaths          <- readr::read_csv("C:\\Personal Data Science Projects\\data\\covid_deaths_usafacts3.csv")

# Check out the data / types
confirmed %>% glimpse()
deaths    %>% glimpse()

# Clean data
covid_cases_county_tbl <- confirmed %>%
      rename(county = `County Name`,
             state_abbrev = State) %>%
      pivot_longer(contains("2020")) %>%
      rename(calendar_date = name,
             confirmed_cases = value) %>%
      mutate(calendar_date = as.Date(calendar_date, "%m/%d/%Y"))

covid_deaths_county_tbl <- deaths %>%
   rename(county = `County Name`,
          state_abbrev = State) %>%
   pivot_longer(contains("2020")) %>%
   rename(calendar_date = name,
          deaths = value) %>%
   mutate(calendar_date = as.Date(calendar_date, "%m/%d/%Y")) 


# Check out new data
covid_cases_county_tbl %>% glimpse()
covid_deaths_county_tbl %>% glimpse()     
   

# Isolate data for latest data only

# Change date to reflect previous day
covid_cases_20200331 <- covid_cases_county_tbl %>%
   filter(calendar_date == max(calendar_date)) %>%
   select(county, state_abbrev, calendar_date, confirmed_cases)
   

covid_deaths_20200331 <- covid_deaths_county_tbl %>%
   filter(calendar_date == max(calendar_date)) %>%
   select(county, state_abbrev, calendar_date, deaths)


# Check total cases to make sure data makes sense
covid_cases_20200331 %>%
   summarise(total_cases = sum(confirmed_cases))

covid_deaths_20200331 %>%
   summarise(total_deaths = sum(deaths))


# Deaths have 2 NAs - need to fix

sum(is.na(covid_deaths_20200331$deaths))

# Replace NAs with 0's
# covid_deaths_20200330[is.na(covid_deaths_20200330)] = 0


# Other ways to check for NAs

# Entire dataframe
# lapply(covid_deaths_20200330,function(x) { length(which(is.na(x)))})

summary(covid_cases_20200331)
summary(covid_deaths_20200331)

# Group confirmed cases by county and state
covid_total_cases_20200331_tbl <- covid_cases_20200331 %>%
   select(county, state_abbrev, confirmed_cases) %>%
   group_by(county, state_abbrev) %>%
   summarise(total_cases = sum(confirmed_cases)) %>%
   ungroup() %>%
   arrange(-total_cases)

# county                state_abbrev    total_cases
# <chr>                 <chr>                 <dbl>
# 1 Queens County          NY                 13869
# 2 Kings County           NY                 11160
# 3 Westchester County     NY                  9967
# 4 Nassau County          NY                  8544
# 5 Bronx County           NY                  7814
# 6 Suffolk County         NY                  6713
# 7 New York County        NY                  6539
# 8 Cook County            IL                  4496
# 9 Wayne County           MI                  3735
# 10 Statewide Unallocated NJ                  3686

# Group deaths by county and state
covid_total_deaths_20200331_tbl <- covid_deaths_20200331 %>%
   select(county, state_abbrev, deaths) %>%
   group_by(county, state_abbrev) %>%
   summarise(total_deaths = sum(deaths)) %>%
   ungroup() %>%
   arrange(-total_deaths)

# A tibble: 3,195 x 3
# county                state_abbrev   total_deaths
# <chr>                 <chr>                 <dbl>
# 1 Queens County         NY                    376
# 2 Bronx County          NY                    262
# 3 Kings County          NY                    261
# 4 Statewide Unallocated NY                    244
# 5 King County           WA                    150
# 6 New York County       NY                    129
# 7 Wayne County          MI                    120
# 8 Orleans Parish        LA                    101
# 9 Oakland County        MI                     70
# 10 Richmond County      NY                     67


# Join deaths to cases
covid_update_20200331_tbl <- covid_total_cases_20200331_tbl %>%
   left_join(covid_total_deaths_20200331_tbl)

# Look at Alaska
covid_update_20200331_tbl %>%
   filter(state_abbrev == "AK") %>%
   # summarise(total_cases = sum(total_cases)) %>%
   arrange(-total_cases)

# Look at Hawaii
covid_update_20200331_tbl %>%
   filter(state_abbrev == "HI") %>%
   # summarise(total_cases = sum(total_cases))
   arrange(-total_cases)

# Look at Alaska
covid_update_20200331_tbl %>%
   filter(state_abbrev == "AK") %>%
   # summarise(total_deaths = sum(total_deaths))
   arrange(-total_deaths)

# Look at Hawaii
covid_update_20200331_tbl %>%
   filter(state_abbrev == "HI") %>%
   # summarise(total_deaths = sum(total_deaths))
   arrange(-total_deaths)
   

# covid_update_20200331_tbl %>%
#    writexl::write_xlsx("C:\\Personal Data Science Projects\\data\\covid_update_20200331.xlsx")



            