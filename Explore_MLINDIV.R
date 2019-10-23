# Author: Robert Woodry
# Contact: rwoodry@uci.edu
# Last Updated: 10/22/2019
# About: Working script for going through a participant's Eprime results file (MLINDIV) and compiling a master data frame 
#         that contains a frame-by-frame update of relevant participant data, in a "tidy data" format. Completed
#         project will output an overall master data frame compiling that data for ALL participants into one .csv file.
#         So far, this script works with participant 002 as an example. 



# Load proper packages. Rprime is built specifically for EPrime
library(rprime)
library(tidyverse)
library(wrapr)

# Set your working directory here. the one below is mine. Copy and paste yours assigned to 'working_dir'
working_dir <- "C:/Users/UCI - Robert Woodry/Desktop/Research/Tasks/MLINDIV/EPrime Experiment Files/Data/BehavPreJustin"
setwd(working_dir)



# Get a list of all text files in Particpant 002's folder. This script is using participant 002 as an example.
eprime_txt_files <- list.files("002", pattern = ".*.txt")
eprime_explore_txt_files <- list.files("002", pattern = "Explore.*.txt")
eprime_test_txt_files <- list.files("002", pattern = "Test.*.txt")

# Create an empty participant master data frame; this will contain all Explore and Test data for a participant.
master_participant <- tibble()


for (file_i in 1:length(eprime_txt_files)){
  
  eprime_txt_files <- list.files("002", pattern = ".*.txt")
  
  # Create data frame, remove header/ender rows and store for later use
  e_file <- read_eprime(paste0("002/", eprime_txt_files[file_i]))
  
  e_frame <- FrameList(e_file)
  e_df <- to_data_frame(e_frame)
  e_df_header <- e_df[1, ]
  e_df <- e_df[2:(nrow(e_df)-1), ]
  
  eprime_test_txt_files <- list.files("002", pattern = "Test.*.txt")
  
  
  

  # If a test trial, create a separate tibble for the TrialProcedures, which contain start/end goals and other variables
  if (eprime_txt_files[file_i] %in% eprime_test_txt_files){
    # Create the trial_proc tibble containing the trial procedure meta-data
    trial_proc <-e_df[e_df$Procedure == "TrialProc" | e_df$Procedure == "TrialRevProc", ]
    trial_proc <- as.tibble(trial_proc) %>%
      select(qc(
        Procedure, Sample, itilist, 
        ITIDur, objlist, ObjDur, 
        pairlist, startPosition, startFacing, 
        StartIm, endPosition, EndIm
      ))
    

    
    # Update the e_df data frame to be a tibble that contains only the variables we care about
    master_tibble <- as.tibble(e_df) %>%
      select(qc(
        Eprime.Basename, Eprime.LevelName,
        ImageFile, Choose.OnsetTime, 
        Choose.RTTime, Choose.RT, 
        VideoFile, MoveVid.OnsetTime
      ))
    
    
    
    # Create an empty tibble where a for loop creates repetitions of trial_proc rows to be used in adding to 
    # the master tibble as new columns ( so for each row, it will now have not only movement data, 
    # but also what trial type it is with Start and end goals, etc)
    trial_cols <- tibble()
    trial_proc_location <- which(e_df$Procedure == "TrialProc" | e_df$Procedure == "TrialRevProc")
    place <- 0
    i = 1
    
    for (row_location in 1:length(trial_proc_location)){
      repeat_num <- trial_proc_location[row_location] - place - 1
      curr_row <- trial_proc[row_location, ]
      new_rows <- curr_row[rep(seq_len(nrow(curr_row)), each = repeat_num), ]
      trial_cols <- rbind(trial_cols, new_rows)
      place <- trial_proc_location[row_location]
      i <- i + 1
     
    }
    
    
    
    
    
    # Remove TrialProc rows from master tibble, as well as the last row (which is useless), then column bind the new 
    # trial_cols(which should now have the same # of rows as the master tibble) to the master tibble
    master_tibble <- master_tibble[-c(trial_proc_location), ]
    master_tibble <- master_tibble[-(nrow(master_tibble)), ]
    master_tibble <- cbind(trial_cols, master_tibble)
    
    
    
    # Create a data frame with same number of rows as master tibble, that contains 3 columns of repeating values: 
    # Subject (participant #), Task (Explore | Test), TaskType (Explore: 1 or 2 | Test: A1...C3)
    Task <- strsplit(e_df_header$DataFile.Basename, "_")[[1]][1]
    Subject <- e_df_header$Subject
    Task_type <- strsplit(e_df_header$DataFile.Basename, "-")[[1]][1]
    Task_type <- paste0(strsplit(Task_type, "_")[[1]][3], strsplit(Task_type, "_")[[1]][4])
    
    
    meta_df <- cbind(Subject, Task, Task_type)
    meta_df <- meta_df[rep(seq_len(nrow(meta_df)), each = nrow(master_tibble)), ]
    
    
    # Bind that meta data frame to the master tibble
    master_tibble <- cbind(meta_df, master_tibble)
    
      
  } else {
    # Update the e_df data frame to be a tibble that contains only the variables we care about
    master_tibble <- as.tibble(e_df) %>%
      select(qc(
        Eprime.Basename, Eprime.LevelName,
        ImageFile, Choose.OnsetTime, 
        Choose.RTTime, Choose.RT, 
        VideoFile, MoveVid.OnsetTime
      ))
    
    # Create a data frame with same number of rows as master tibble, that contains 3 columns of repeating values: 
    # Subject (participant #), Task (Explore | Test), TaskType (Explore: 1 or 2 | Test: A1...C3)
    Task <- strsplit(e_df_header$DataFile.Basename, "-")[[1]][1]
    Subject <- e_df_header$Subject
    Task_type <- strsplit(e_df_header$DataFile.Basename, "-")[[1]][3]
    Procedure <- NA
    Sample <- NA
    itilist <- NA
    ITIDur <- NA
    objlist <- NA
    ObjDur <- NA
    pairlist <- NA
    startPosition <- NA
    startFacing <- NA
    StartIm <- NA
    endPosition <- NA
    EndIm <- NA
    
    
    meta_df <- cbind(Subject, Task, Task_type, Procedure, Sample, itilist, ITIDur, objlist, ObjDur, pairlist, startPosition, startFacing, StartIm, endPosition, EndIm)
    meta_df <- meta_df[rep(seq_len(nrow(meta_df)), each = nrow(master_tibble)), ]
    
    
    # Bind that meta data frame to the master tibble
    master_tibble <- cbind(meta_df, master_tibble)
    
      
    }
  # Append master_tibble to participant_master file through rbind()
  master_participant <- rbind(master_participant, master_tibble)
  
}

write.csv(master_participant, file ="master.csv")




