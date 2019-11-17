
# Load libraries needed (we only need 2)
library(rvest)
library(tidyverse)

# Use read_html() function to read in the name of the file
# Best to make sure it is stored locally where your R files are stored
# Reminder -- after copying path from your file source page menu, make sure to adjust slashes (forward slashes)
webpage <- read_html("C:/Users/jenni/R/most_valuable_brands.html")

# Use html_nodes() to grab only the html stored as a table or <table>
tbl <-  html_nodes(webpage, "table")

# Review first few rows of results
head(tbl)

# Grab only the portion that applies to your list or table (see "18" below as an example)
# Tip: set fill = TRUE 
tbl2 <- tbl %>%
    .[18] %>%
    html_table(fill = TRUE)

# Use as.data.frame to convert results to a dataframe, removing any columns not needed
# My results included a "Var.1" column with NAs, so I remove that.
final_df <- as.data.frame(tbl2) %>%
    select(-"Var.1")