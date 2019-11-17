library(rvest)
library(tidyverse)


webpage <- read_html("C:/Users/jenni/R/most_valuable_brands.html")

tbl <-  html_nodes(webpage, "table")

head(tbl)

tbl2 <- tbl %>%
    .[18] %>%
    html_table(fill = TRUE)

final_df <- as.data.frame(tbl2) %>%
    select(-"Var.1")