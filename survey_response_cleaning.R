#Script for reading Google Cloud Surveys

library(usethis)
library('data.table')
library(cloudml)
library(readr)
library(googleCloudStorageR)

gcs_global_bucket("bcc-bridge-survey")
proj <- 'practice-gcp-310913'
buckets <- gcs_list_buckets(proj)
bucket <- "bcc-bridge-survey"
bucket_info <- gcs_get_bucket(bucket)
objects <- gcs_list_objects()
gcs_get_object(objects$name[[1]], saveToDisk = "bcc-bridge-survey-working.csv")



