# Author: Robert Woodry
# Contact: rwoodry@uci.edu
# Last Updated: 11/01/2019
# About: This script goes through all participant file folders, compiles relevant Eprime behavioral data into a 
#         "tidy data" format, and appends it to a MLINDIV_behavioral_master.csv for further analysis


# Load proper packages. Rprime is built specifically for EPrime. If you do not have these packages installed, use
#     install.packages("[package name goes here]") to install it on your R client.
library(rprime)
library(tidyverse)
library(wrapr)
library(plyr)

# Set your working directory here. the one below is mine. Copy and paste yours, and assign it to 'working_dir'. 
# This working directory should contain file folders for each participant, with each folder containing Eprime trial data.
# Each folder is named with participant's number:
# i.e.: "002.....103"
working_dir <- "C:/Users/UCI - Robert Woodry/Desktop/Research/Tasks/MLINDIV/EPrime Experiment Files/Data/BehavPreJustin"
setwd(working_dir)

# Create a list of file folders containing participant data
MLINDIV_filelist <- list.files()
MLINDIV_filelist <- MLINDIV_filelist[nchar(MLINDIV_filelist) == 3]

# Create an empty master data frame; this will contain all Explore and Test data for each participant.

master_file <- tibble()

# Loop through each participant's folder
for (participant_file_folder in 1:length(MLINDIV_filelist)){
  current_file <- MLINDIV_filelist[participant_file_folder]

  master_participant <- tibble()
  eprime_txt_files <- list.files(MLINDIV_filelist[participant_file_folder], pattern = ".*.txt")
  
  # Loop through each .txt file in that participant's folder, build a master participant file, to be appended
  # to the mater file at the end of loop
  for (file_i in 1:length(eprime_txt_files)){
    
    
    eprime_txt_files <- list.files(MLINDIV_filelist[participant_file_folder], pattern = ".*.txt")
    
    # Create data frame, remove header/ender rows and store for later use
    e_file <- read_eprime(paste0(MLINDIV_filelist[participant_file_folder], "/" , eprime_txt_files[file_i]))
    
    # Check to see if file is complete
    if (last(e_file) != "*** LogFrame End ***"){
      print(paste(eprime_txt_files[file_i], "skipped", "incomplete file"))
      next
    }
    e_frame <- FrameList(e_file)
    e_df <- to_data_frame(e_frame)
    
    # Check to see if file contains minimum amount of rows for compilation
    if (nrow(e_df) < 10){
      print(paste(eprime_txt_files[file_i], "skipped", "row #", nrow(e_df)))
      next
    }
    e_df_header <- e_df[1, ]
    e_df <- e_df[2:(nrow(e_df)-1), ]
    
    eprime_test_txt_files <- list.files(MLINDIV_filelist[participant_file_folder], pattern = "Test.*.txt")
    print(eprime_txt_files[file_i])
    
    
    
    
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
      
        
    } 
    else {
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
  
  # Append master_participant file to the master_file
  master_file <- rbind(master_file, master_participant)
  print(current_file)
}

convert_to_pos <- function (imagecol){
  end_pos = c()
  for (i in 1:length(imagecol)){
    pos <- strsplit(as.character(imagecol[i]), "/")
    pos <- pos[[1]][2]
    pos <- strsplit(pos, ".jpg")
    pos <- pos[[1]][1]
    # TODO:  If string contains "circle", split
    end_pos <- c(end_pos, pos)
    
  }
  return(end_pos)
}

convert_to_hall <- function(videocol){
  hallsnip <- c()
  for (i in 1:length(videocol)){
    hall <- substr(videocol[i], 8, 12)
    hallsnip <- c(hallsnip, hall)
  }
  return(hallsnip)
}

convert_to_movement <- function(hallcol){
  movement <- c()
  for ( i in 1:length(hallcol)){
    
    mov <- substr(hallcol[i], 1, 1) == substr(hallcol[i], 4, 4)
    if (mov & !is.na(mov)){
      mov <- "Rot"
    } else if (hallcol[i] == "Selec" & !is.na(mov)){
      mov <- "Select"
    } else if (!is.na(mov)){
      mov <- "Walk"
    }
    movement <- c(movement, mov)
  }
  return(movement)
}

convert_to_letter <- function(endloccol){
  lett <- c()
  for (i in 1:length(endloccol)){
    let_loc <- substr(endloccol[i], 1, 1)
    lett <- c(lett, let_loc)
  }
  return(lett)
}

convert_to_dir <- function(endloccol){
  direction <- c()
  for (i in 1:length(endloccol)){
    dirnum <- as.numeric(substr(endloccol[i], 2, 2))
    if (!is.na(dirnum)){
      face_dir <- switch(dirnum, "N", "E", "S", "W")
    } else {
      face_dir <- dirnum
    }
    
    direction <- c(direction, face_dir)
  }
  return(direction)
}

master_file <- master_file %>% mutate(end_location = convert_to_pos(ImageFile))
master_file <- master_file %>% mutate(hallsnip = convert_to_hall(VideoFile))
master_file <- master_file %>% mutate(movement = convert_to_movement(hallsnip))
master_file <- master_file %>% mutate(letter_loc = convert_to_letter(end_location))
master_file <- master_file %>% mutate(face_dir = convert_to_dir(end_location))
master_file <- master_file %>% mutate(sphere = grepl("sphere", master_file$end_location))

master_file <- join(master_file, mcoords, by = "letter_loc")

for (i in 1:length(master_file$letter_loc)){
  if (master_file$letter_loc[i] == "L" & !is.na(master_file$letter_loc[i])){
    master_file$x[i] <- -5
    master_file$y[i] <- 0
  }
}


# Export the finished product
write.csv(master_file, "MLINDIV_behavioral_master.csv")









