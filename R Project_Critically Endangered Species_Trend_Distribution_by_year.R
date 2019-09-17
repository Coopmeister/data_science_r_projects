########################INTRODUCTION#############################
# The purpose of this file is to demonstrate how to scrape
# tables from a PDF file. To simplify the example, the 
# below code will only extract the first table in the document
########################START####################################

# Step 1:  Install and import appropriate packages ----

# Need to make sure Java installed and know where it is located
# Copy / paste path below next to JAVA_HOME
# Need to install rJava to use "tabulizer"

install.packages("rJava")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_221')
library(rJava)

install.packages("tabulizer")
library(tabulizer)

# Additional libraries neeeded for data cleaning and graph
library(tidyverse)
library(tidyr)
library(reshape2)
library(plyr)
library(ggplot2)

# Step 2: Import PDF and use extract_tables function from "tabulizer" to scrape first table ----

# Declare a variable called "es" (short for "endangered species") to represent path of pdf file
es = 'C:/Users/jenni/Desktop/DS4B_101_R_Business_Analysis/endangered_species.pdf'

es2 <- extract_tables(es, method = "decide", output = "data.frame")

req_table <- es2[[1]]

# Convert extracted results to a dataframe or tibble and use "T" for True to maintain header or columns
req_table2 <- as.data.frame(req_table, header=T)

# Step 3:  Create a function to make sure we maingain the integrity of the original columns ----
header_true <- function(df) {
      names(df) <- as.character(unlist(df[1,]))
      df[-1,]
}

# Step 4:  Apply function ----
final_es <- header_true(req_table2)

# Step 5:  Check results ----
final_es %>% view()

# Step 6:  Perform some data cleaning ----

# Filter out columns with NAs
df <- Filter(function(x)!all(is.na(x)),final_es)

# Remove column 5, as the extract_tables process misaggregated the columns (3 columns combined)
# Note:  Was unable to find a way to resolve this, so deleting this column
# (e.g., Results will not include Amphibians, Fishes and Insects)

final_df <- df[-c(5)]

# Convert wide format to long format for graphing
critically_endangered_tbl <- gather(final_df,key=species,value=number,-Year)


# Step 7:  Do final prep for graph ----
ce_final_tbl <- critically_endangered_tbl %>%
      mutate(number = gsub(",","",number)) %>%
      mutate(number_num = as.numeric(number)) %>%
      select(Year,species,number_num) %>%
      mutate(percent = number_num/sum(number_num)*100) %>%
      mutate(label = scales::percent(percent))

# Step 8 (Final step):  Create stacked bar chart to show growth and distribution of critically endangered species numbers by species ----
ggplot(ce_final_tbl, aes(x = factor(Year), y = percent, fill = species)) +
      geom_bar(position = position_stack(), stat = "identity", width = .7) +
      geom_text(aes(label = label), position = position_stack(vjust= 0.5), size = 2) +
      coord_flip()

