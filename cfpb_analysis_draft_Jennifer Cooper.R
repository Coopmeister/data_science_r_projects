
# 1.0  Business problem ----

# Healthcare is increasingly putting the patient/ consumer in focus. While there are existing ways/ sources
# providing a means to file / store / access a complaint against a provider, the purpose of this analysis
# is to examine consumer complaints regarding financial products and services to generate a summary / 
# insight report that a company in the Dallas-Fort Worth area could use to glean value (e.g., get a general
# idea of the number and type of complaints consumers may file, as well as the channels they use, 
# and how companies respond), based on Consumer Financial Protection Bureau (CFPB) Consumer Complaint data.

# 1.1 Assumption(s):  Results could be used to benchmark number / type of complaints made and potentially 
# improve responses to patient / consumer complaints in healthcare 


# 1.2 Objective(s) and Scope:

#     a. See if there is any data about consumer complaints by industry to gain context
#        https://consumerfed.org/press_release/nations-top-consumer-complaints-2/
#        https://consumerfed.org/wp-content/uploads/2019/07/Top-Consumer-Complaints-Report.pdf

#     b. Examine CFPB data:
#        1.) Summarize complaints by product
#        2.) Review the types of complaints (e.g., issues) 
#        3.) Take a look at complaints by channel and state to detect any interesting patterns
#        4.) Assess company response
#     c. Scope:  
#        1.) Time investment: ~ <= 8 hours (Will keep things high level)
#        2.) Deliverable:  Code, Report (PDF, HTML, etc.); any instructions, supplemental data,
#        needed to reproduce results.




# 2.0  Get the data ----

# 2.1  Import libraries ----

library("tidyverse")
library("readxl")
# install.packages("DataExplorer")
library("DataExplorer")
library("tidyquant")
library("ggrepel")


# 2.2  Read in CFPB Complaint file (This takes a little time) ----

# Source: https://catalog.data.gov/dataset/consumer-complaint-database#topic=consumer_navigation

# Additional info:  https://www.consumerfinance.gov/data-research/consumer-complaints/

cfpb <- read.csv("C:\\Users\\jenni\\Desktop\\Job Search 2020\\Interview Prep\\Baylor\\complaints.csv", stringsAsFactors = FALSE)
# Note:  Data last updated -- January 27, 2020

# 2.3  Review data dictionary
#      https://cfpb.github.io/api/ccdb/fields.html


# 3.0  Prep data ----

# Take a peek of all fields in the data & record number of variables and observations

glimpse(cfpb)


#    Observations:   There are 19 variables or columns   
#         Records:   There are 1,531,308 obesrvations    

# Since there's a large amount of data, I'm going to take a sample.

set.seed(12345)
cfpb_2 <- sample_n(cfpb,15000)


# Get to know the data (look for any missing values and plot stats, as well as product complaint frequency)

introduce(cfpb_2)
#Note:   No missing values

plot_intro(cfpb_2)
#Note:   Most columns are discrete


# Rename columns and isolate fields needed; convert some to factors to make for easier graphing
cfpb_sample <- cfpb_2 %>%
    rename(date_received                   = Date.received,
           financial_product               = Product,
           financial_subproduct            = Sub.product,
           consumer_issue                  = Issue,
           consumer_subissue               = Sub.issue,
           consumer_complaint_comments     = Consumer.complaint.narrative,
           company_public_response         = Company.public.response,
           company                         = Company,
           consumer_state                  = State,
           consumer_zip                    = ZIP.code,
           complaint_search_tag            = Tags,
           consumer_publication_consent    = Consumer.consent.provided.,
           complaint_submission_channel    = Submitted.via,
           date_sent_to_company            = Date.sent.to.company,
           company_complaint_response      = Company.response.to.consumer,
           company_timely_response_flag    = Timely.response.,
           consumer_response_disputed      = Consumer.disputed.,
           complaint_id                    = Complaint.ID) %>%
    
    # In the interest of time, going to focus on high level categories, not "sub" categories
    select(complaint_id,
           financial_product, 
           consumer_issue, 
           company_public_response,
           company_complaint_response,
           consumer_state,
           complaint_submission_channel
    )  


glimpse(cfpb_sample)



# 4.0  EDA and Analysis ----

# See what financial products are receiving the most complaints
cfpb_sample %>%
    select(financial_product, complaint_id) %>%
    group_by(financial_product) %>%
    summarise(total_complaints = n()) %>%
    ungroup() %>%
    arrange(-total_complaints) %>%
    head(10) %>%
    mutate(financial_product = financial_product %>% as_factor() %>% fct_reorder(total_complaints)) %>%
    
    ggplot(aes(financial_product, total_complaints)) +
    geom_col(fill = "#2c3e50") +
    ggrepel::geom_label_repel(aes(label = total_complaints), size = 5) +
    theme_tq() +
    labs(title    = "Top 10 Financial Products by Number of Complaints",
         subtitle = "The financial product receiving the most complaints is 'Credit Bureau Services'",
         x        = "Total Complaints",
         y        = "Financial Product") +
    coord_flip()


# Identify the biggest issues with the finacial products
cfpb_sample %>%
    select(consumer_issue, complaint_id) %>%
    group_by(consumer_issue) %>%
    summarise(total_complaints = n()) %>%
    ungroup() %>%
    arrange(-total_complaints) %>%
    head(10) %>%
    mutate(consumer_issue = consumer_issue %>% as_factor() %>% fct_reorder(total_complaints)) %>%
    
    ggplot(aes(consumer_issue, total_complaints)) +
    geom_col(fill = "#2c3e50") +
    ggrepel::geom_label_repel(aes(label = total_complaints), size = 5) +
    theme_tq() +
    labs(title    = "Top 10 Consumer Issues by Number of Complaints",
         subtitle = "The biggest issue consumers have is 'Incorrect information on their report'",
         x        = "Total Complaints",
         y        = "Consumer Issue") +
    coord_flip()

# Examine complaints by channel
cfpb_sample %>%
    select(complaint_submission_channel, complaint_id) %>%
    group_by(complaint_submission_channel) %>%
    summarise(total_complaints = n()) %>%
    ungroup() %>%
    arrange(-total_complaints) %>%
    mutate(complaint_submission_channel = complaint_submission_channel %>% as_factor() %>% fct_reorder(total_complaints)) %>%
    
    ggplot(aes(complaint_submission_channel, total_complaints)) +
    geom_col(fill = "#2c3e50") +
    ggrepel::geom_label_repel(aes(label = total_complaints), size = 5) +
    theme_tq() +
    labs(title    = "Consumer Complaints by Channel",
         subtitle = "Most complaints are submitted through the Web",
         x        = "Total Complaints",
         y        = "Channel") +
    coord_flip()


# Analyze company responses to consumer complaints
cfpb_sample %>%
    select(company_complaint_response, complaint_id) %>%
    group_by(company_complaint_response) %>%
    summarise(total_complaints = n()) %>%
    ungroup() %>%
    arrange(-total_complaints) %>%
    head(10) %>%
    mutate(company_complaint_response = company_complaint_response %>% as_factor() %>% fct_reorder(total_complaints)) %>%
    
    ggplot(aes(company_complaint_response, total_complaints)) +
    geom_col(fill = "#2c3e50") +
    ggrepel::geom_label_repel(aes(label = total_complaints), size = 5) +
    theme_tq() +
    labs(title    = "Top 10 Company Responses by Number of Complaints",
         subtitle = "The most popular company response is 'Closed with explanation', followed by 'Closed with Non-monetary Relief' and 'Closed with Monetary Relief'",
         x        = "Total Complaints",
         y        = "Company Response") +
    coord_flip()


# Check out complaints by state to see if anything stands out
cfpb_sample %>%
    select(consumer_state, complaint_id) %>%
    group_by(consumer_state) %>%
    summarise(total_complaints = n()) %>%
    ungroup() %>%
    arrange(-total_complaints) %>%
    head(10) %>%
    mutate(consumer_state = consumer_state %>% as_factor() %>% fct_reorder(total_complaints)) %>%
    
    ggplot(aes(consumer_state, total_complaints)) +
    geom_col(fill = "#2c3e50") +
    ggrepel::geom_label_repel(aes(label = total_complaints), size = 5) +
    theme_tq() +
    labs(title    = "Top 10 States by Number of Complaints",
         subtitle = "Most complaints are coming from California, followed by Florida, Texas, New York and Georgia",
         x        = "Total Complaints",
         y        = "Company Response") +
    coord_flip()

