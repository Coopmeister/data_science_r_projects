library(tidyverse)
library(tidyquant)
library(lubridate)
library(ggplot2)
library(gganimate)
library(gifski)
library(hrbrthemes)


cpi_tuition <- read_csv("C:/Users/jenni/OneDrive/Documents/Personal Data Science Projects/7DaysofAnimation/1. Original Data/consumer_price_index_tuition.csv")

cpi_tuition %>% glimpse()

cpi_tuition_tbl <- cpi_tuition %>%
      rename(all_items = `All items`,
             college_tuition_and_fees = `College tuition and fees`,
             elementary_and_hs_tuition_and_fees = `Elementary and high school tuition and fees`,
             child_care_and_nursery_school = `Child care and nursery school`,
             technical_and_business_school_tuition_and_fees = `Technical and business school tuition and fees`,
             housing_at_school_excluding_board = `Housing at school, excluding board`,
             college_textbooks = `College textbooks`) 
 
cpi_tuition_clean_tbl <- cpi_tuition_tbl %>%     
      pivot_longer(c(all_items,college_tuition_and_fees,elementary_and_hs_tuition_and_fees,child_care_and_nursery_school,technical_and_business_school_tuition_and_fees,housing_at_school_excluding_board,college_textbooks)) %>%
      rename(month_year = "Month",
             category = "name",
             index_value = "value") %>%
      mutate(mo_yr = as.Date(paste("01",month_year,sep="-"),"%d-%b-%y")) %>%
      select(-month_year) %>%
      select(mo_yr,category,index_value)

cpi_tuition_clean_tbl %>%
      ggplot( aes(x=mo_yr, y=index_value, group=index_value, color=category)) +
      geom_point() +
      ggtitle("College tuition and fees have increased 63% since 2006") +
      # theme_ipsum() +
      theme_tq() +
      scale_color_tq() +
      ylab("Index") +
      xlab("") +
      theme(plot.title = element_text(size = 14),
            legend.position = "bottom", legend.direction = "vertical",
            axis.title.x = element_text(hjust = 0.5),
            axis.title.y = element_text(hjust = 0.5),
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 8, angle = 45)) +
      transition_reveal(mo_yr)


