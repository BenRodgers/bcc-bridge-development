# Script for reading Google Cloud Surveys
library(dplyr)
library(usethis)
library('data.table')
library(cloudml)
library(readr)
library(googleCloudStorageR)
library(tidyverse)
library(rjson)
library(ggplot2)
library(zoo)
library(textcat)
library("shiny")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")

# Download the results from the survey from Google Cloud Survey
gcs_global_bucket("bcc-bridge-survey")
proj <- 'practice-gcp-310913'
buckets <- gcs_list_buckets(proj)
bucket <- "bcc-bridge-survey"
bucket_info <- gcs_get_bucket(bucket)
objects <- gcs_list_objects()
gcs_get_object(objects$name[[1]], saveToDisk = "bcc-bridge-survey-working.csv")

# Read data, create tibble and perform initial EDA
df <-read_csv('data/bcc-bridge-survey-working.csv')
as_tibble(df)
glimpse(df)

#Add classification columns for each question
df$question_1_type='contact_quantity'
df$question_2_type='contact_quantity'
df$question_3_type='social_infrastructure'
df$question_4_type='contact_quality'
df$question_5_type='contact_quality'
df$question_6_type='procedural_fairness'
df$question_7_type='procedural_fairness'
df$question_8_type='procedural_fairness'
df$question_9_type='trust'
df$question_10_type='acceptance'
df$question_11_type='response'
df$question_12_type='response'
df$question_1_enumerate = 0
df$question_2_enumerate = 0
df$question_3_enumerate = 0
df$question_4_enumerate = 0
df$question_5_enumerate = 0
df$question_6_enumerate = 0
df$question_7_enumerate = 0
df$question_8_enumerate = 0
df$question_9_enumerate = 0
df$question_10_enumerate = 0

# Get Column Indexes
as.data.frame(colnames(df))

#Enumerate responses to (0, 1, 2, 3, 4, 5) for Question 1
for(i in 2:nrow(df)) {
  element <- df[i,4]
  if(element == 'None at all'){
    df[i, 26] <- 1
  }
  if(element == 'A little'){
    df[i, 26] <- 2
  }
  if(element == 'A moderate amount'){
    df[i, 26] <- 3
  }
  if(element == 'A lot'){
    df[i, 26] <- 4
  }
  if(element == 'A great deal'){
    df[i, 26] <- 5
  }
}

#Enumerate responses to (0, 1, 2, 3, 4, 5) for Question 2
for(i in 2:nrow(df)) {
  element <- df[i,5]
  if(element == 'None at all'){
    df[i, 27] <- 1
  }
  if(element == 'A little'){
    df[i, 27] <- 2
  }
  if(element == 'A moderate amount'){
    df[i, 27] <- 3
  }
  if(element == 'A lot'){
    df[i, 27] <- 4
  }
  if(element == 'A great deal'){
    df[i, 27] <- 5
  }
}

#Enumerate responses to (0, 1, 2, 3, 4, 5) for Question 3
for(i in 2:nrow(df)) {
  element <- df[i,6]
  if(element == 'None at all'){
    df[i, 28] <- 1
  }
  if(element == 'A little'){
    df[i, 28] <- 2
  }
  if(element == 'A moderate amount'){
    df[i, 28] <- 3
  }
  if(element == 'A lot'){
    df[i, 28] <- 4
  }
  if(element == 'A great deal'){
    df[i, 28] <- 5
  }
}

#Enumerate responses to (0, 1, 2, 3, 4, 5) for Question 4
for(i in 2:nrow(df)) {
  element <- df[i,7]
  if(element == 'Strongly disagree'){
    df[i, 29] <- 1
  }
  if(element == 'Somewhat disagree'){
    df[i, 29] <- 2
  }
  if(element == 'Neither agree nor disagree'){
    df[i, 29] <- 3
  }
  if(element == 'Somewhat agree'){
    df[i, 29] <- 4
  }
  if(element == 'Strongly agree'){
    df[i, 29] <- 5
  }
}


#Enumerate responses to (0, 1, 2, 3, 4, 5) for Question 5
for(i in 2:nrow(df)) {
  element <- df[i,8]
  if(element == 'Strongly disagree'){
    df[i, 30] <- 1
  }
  if(element == 'Somewhat disagree'){
    df[i, 30] <- 2
  }
  if(element == 'Neither agree nor disagree'){
    df[i, 30] <- 3
  }
  if(element == 'Somewhat agree'){
    df[i, 30] <- 4
  }
  if(element == 'Strongly agree'){
    df[i, 39] <- 5
  }
}

#Enumerate responses to (0, 1, 2, 3, 4, 5) for Question 6
for(i in 2:nrow(df)) {
  element <- df[i,9]
  if(element == 'Strongly disagree'){
    df[i, 31] <- 1
  }
  if(element == 'Somewhat disagree'){
    df[i, 31] <- 2
  }
  if(element == 'Neither agree nor disagree'){
    df[i, 31] <- 3
  }
  if(element == 'Somewhat agree'){
    df[i, 31] <- 4
  }
  if(element == 'Strongly agree'){
    df[31] <- 5
  }
}

#Enumerate responses to (0, 1, 2, 3, 4, 5) for Question 7
for(i in 2:nrow(df)) {
  element <- df[i,10]
  if(element == 'Strongly disagree'){
    df[i, 32] <- 1
  }
  if(element == 'Somewhat disagree'){
    df[i, 32] <- 2
  }
  if(element == 'Neither agree nor disagree'){
    df[i, 32] <- 3
  }
  if(element == 'Somewhat agree'){
    df[i, 32] <- 4
  }
  if(element == 'Strongly agree'){
    df[i, 32] <- 5
  }
}

#Enumerate responses to (0, 1, 2, 3, 4, 5) for Question 8
for(i in 2:nrow(df)) {
  element <- df[i,11]
  if(element == 'Strongly disagree'){
    df[i, 33] <- 1
  }
  if(element == 'Somewhat disagree'){
    df[i, 33] <- 2
  }
  if(element == 'Neither agree nor disagree'){
    df[i, 33] <- 3
  }
  if(element == 'Somewhat agree'){
    df[i, 33] <- 4
  }
  if(element == 'Strongly agree'){
    df[i, 33] <- 5
  }
}

#Enumerate responses to (0, 1, 2, 3, 4, 5) for Question 9
for(i in 2:nrow(df)) {
  element <- df[i,12]
  if(element == 'Strongly disagree'){
    df[i, 34] <- 1
  }
  if(element == 'Somewhat disagree'){
    df[i, 34] <- 2
  }
  if(element == 'Neither agree nor disagree'){
    df[i, 34] <- 3
  }
  if(element == 'Somewhat agree'){
    df[i, 34] <- 4
  }
  if(element == 'Strongly agree'){
    df[i, 34] <- 5
  }
}

#Enumerate responses to (0, 1, 2, 3, 4, 5) for Question 10
for(i in 2:nrow(df)) {
  element <- df[i,13]
  if(element == 'Strongly disagree'){
    df[i, 35] <- 1
  }
  if(element == 'Somewhat disagree'){
    df[i, 35] <- 2
  }
  if(element == 'Neither agree nor disagree'){
    df[i, 35] <- 3
  }
  if(element == 'Somewhat agree'){
    df[i, 35] <- 4
  }
  if(element == 'Strongly agree'){
    df[i, 35] <- 5
  }
}

#Remove any duplicated data (ideally have a key column like a hashed timestamp) and drop na (assume mandatory responses)
df %>% drop_na()
df %>% distinct()

write.csv(df,"../bridge-survey/data/bcc-bridge-survey-final.csv", row.names = FALSE)
