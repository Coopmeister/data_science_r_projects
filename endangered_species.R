install.packages("rJava")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_221')
library(rJava)

install.packages("tabulizer")
library(tabulizer)
library(tidyverse)
library(tidyr)
library(reshape2)
library(plyr)
library(ggplot2)


es = 'C:/Users/jenni/Desktop/DS4B_101_R_Business_Analysis/endangered_species.pdf'

# Extract just the first table
es2 <- extract_tables(es, method = "decide", output = "data.frame")

req_table <- es2[[1]]

req_table2 <- as.data.frame(req_table, header=T)


header_true <- function(df) {
    names(df) <- as.character(unlist(df[1,]))
    df[-1,]
}

final_es <- header_true(req_table2)

final_es


# Split endangered_species.pdf (now "es") into separate pdfs, per # of tables)

split_pdf(es)

# results are saved as values to for extraction

es_1 = "C:\\Users\\jenni\\AppData\\Local\\Temp\\RtmpQpLXr3\\endangered_species1.pdf"
es_2 = "C:\\Users\\jenni\\AppData\\Local\\Temp\\RtmpQpLXr3\\endangered_species2.pdf"
es_3 = "C:\\Users\\jenni\\AppData\\Local\\Temp\\RtmpQpLXr3\\endangered_species3.pdf"



# Extract tables from value 1
es_1_tbl <- extract_tables(es_1, method = "decide", output = "data.frame")
req_table_es_1 <- es_1_tbl[[1]] 

req_table_es_1_final<- as.data.frame(req_table_es_1, header=T)

# Apply this function to maintain integrity of columns
header_true <- function(df) {
    names(df) <- as.character(unlist(df[1,]))
    df[-1,]
}

final_es1 <- header_true(req_table_es_1_final)

final_es %>% view()

    
df <- Filter(function(x)!all(is.na(x)),final_es)


final_df <- df[-c(5)]

critically_endangered_tbl <- gather(final_df,key=species,value=number,-Year)
    
ce_final_tbl <- critically_endangered_tbl %>%
    mutate(number = gsub(",","",number)) %>%
    mutate(number_num = as.numeric(number)) %>%
    select(Year,species,number_num) %>%
    mutate(percent = number_num/sum(number_num)*100) %>%
    mutate(label = scales::percent(percent))

ggplot(ce_final_tbl, aes(x = factor(Year), y = percent, fill = species)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = label), position = position_stack(vjust= 0.5), size = 2) +
    coord_flip()
    








# Extract tables from value 2
es_2_tbl <- extract_tables(es_2, method = "decide", output = "data.frame")
req_table_es_2 <- es_2_tbl[[1]] 

req_table_es_2_final <- as.data.frame(req_table_es_2, header=T)

# Apply this function to maintain integrity of columns
header_true <- function(df) {
    names(df) <- as.character(unlist(df[1,]))
    df[-1,]
}

final_es2 <- header_true(req_table_es_2_final)

final_es2



# Extract tables from value 2
es_3_tbl <- extract_tables(es_3, method = "decide", output = "data.frame")
req_table_es_3 <- es_3_tbl[[1]] 

req_table_es_3_final <- as.data.frame(req_table_es_3, header=T)

# Apply this function to maintain integrity of columns
header_true <- function(df) {
    names(df) <- as.character(unlist(df[1,]))
    df[-1,]
}

final_es3 <- header_true(req_table_es_3_final)




