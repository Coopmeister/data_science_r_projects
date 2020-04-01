library(tidyverse)
library(writexl)
library("rvest")

url <- "https://www.worldometers.info/coronavirus/"

coronavirus <- url %>%
      xml2::read_html() %>%
      # html_nodes(xpath='//*[@id="main_table_countries"]') %>%
      html_nodes(xpath='//*[@id="main_table_countries_today"]') %>%
      html_table()

covid <- coronavirus[[1]]

covid %>%
      filter(`Country,Other` == "China")
