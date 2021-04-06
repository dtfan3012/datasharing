##This script takes the data available in the UCI HAR dataset and makes 
## it tidy as well as subsets different activities that measure the mean
## and standard dev for each of the derived measurements.  

  setwd(getwd()) ## user directory is set here for downloading the zip file

## Raw data file is downloaded and unzipped and set as new working dir    
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url, destfile = "./uci_raw_data.zip", method = "curl")
  unzip("uci_raw_data.zip")
  
## Unzipped folder is set as new working directory  
  setwd("./UCI HAR Dataset")
  
## read in the x_test.txt and x_train.txt files 
## as xtest and xtrain data tables
  xtest <- read.table("./test/X_test.txt", header = FALSE)
  xtrain <- read.table("./train/X_train.txt", header = FALSE)
  
## read in the list of variables calculated totaling 561 names var_set   
  var_set <- read.table("./features.txt", header = FALSE)

## read in vectors for the subject_id for both train and test data
  sub_test <- read.table("./test/subject_test.txt")
  sub_train <- read.table("./train/subject_train.txt")
  
## read in vectors for test & train data numeric for
## each of the 6 activities used for the experiment
  acts_test <- read.table("./test/y_test.txt")
  acts_train <- read.table("./train/y_train.txt")
  
## combine the xtest & xtrain to a single set called df_values
## combine the sub_test & sub_train to single set called df_sub
## combine the acts_test & acts_train to a single set called df_acts  
  df_values <- rbind(xtest, xtrain)
  df_sub <- rbind(sub_test, sub_train)
  df_acts <- rbind(acts_test, acts_train)
  
## Rename columns using var_set for dt_values assigning variable  
## vector as 'df_values_names'
## Rename column for df_subs as 'subject_id'
## Rename column for df_acts as 'activities'
  df_values_names <- var_set$V2
  colnames(df_values) <- df_values_names
  
  df_sub <- rename(df_sub, subject_id = V1)
  
  df_acts <- rename(df_acts, activities = V1)
  
## This section changes the numeric values in activities to easier to  
## identify and understand names that represent each experiment
  df_acts <- df_acts %>% 
    select(activities) %>%
    transmute(case_when(activities == 5 ~ "standing",
                     activities == 4 ~ "sitting",
                     activities == 3 ~ "walk_down",
                     activities == 2 ~ "walk_up",
                     activities == 1 ~ "walking",
                     activities == 6 ~ "laying")
    )
  
  df_acts <- rename(df_acts, actions = `case_when(...)`)
  
## Finally the three sets are combined with a data.frame dim = 10299 x 563
  
  df_total <- cbind(df_sub, df_acts, df_values)
  
## Subset of df_total filtering out subject_id, actions, and any column 
## variables that calculate the mean or std deviation 
  
  df_mean_sd <- select(df_total, subject_id, actions, 
                       matches("mean()"), matches("std()"))
  
## make an independent copy of dt_mean_sd to group_by subject or action
  df_temp <- data.frame(df_mean_sd)

## both lines below make df with dim 180 x 88 giving the mean values for each
## subject's data and mean values for each experient across the subjects
## and then selects out just the mean column with matches argument used
## previously to select from the full set of variables.
  
  df_actions_set <- df_temp %>%
    group_by(actions, subject_id) %>%
    summarize(across(everything(),mean)) %>% 
    select(subject_id, matches("mean()"))
    
  df_subject_id_set <- df_temp %>%
    group_by(subject_id, actions) %>%
    summarize(across(everything(),mean)) %>% 
    select(actions, matches("mean()"))

## Both csv file below are written to the unzipped folder 
## of the UCI dataset and they both contain headers  
  write.table(df_actions_set, 
              file = "./Actions Mean Summary.csv") 
  write.table(df_subject_id_set, 
              file = "./Subject's Mean Summary.csv")

## Clear environment with just the exported csv files
  rm(list = ls()); gc()


