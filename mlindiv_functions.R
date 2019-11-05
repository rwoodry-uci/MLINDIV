
library(rprime)
library(wrapr)
library(tidyverse)
library(plyr)

working_dir <- "C:/Users/UCI - Robert Woodry/Desktop/Research/Tasks/MLINDIV/EPrime Experiment Files/Data/BehavPreJustin"
setwd(working_dir)

two <- to_data_frame(FrameList(read_eprime("002/Test_Trials_B_1-002-1.txt")))
two <- two %>%
  select(qc(
    Eprime.Basename, Eprime.LevelName,
    ImageFile, Choose.OnsetTime, 
    Choose.RTTime, Choose.RT, 
    VideoFile, MoveVid.OnsetTime
  ))

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

endloc_fix <- function(endloccol){
  end_location <- c()
  for (i in 1:length(endloccol)){
    if (as.character(endloccol[i]) == "V3" & !is.na(endloccol[i])){
      el <- "Y3"
    } else if (as.character(endloccol[i]) == "V4" & !is.na(endloccol[i])){
      el <- "Y4"
    } else if (as.character(endloccol[i]) == "V3_sphere" & !is.na(endloccol[i])){
      el <- "Y3_sphere"
    } else {
      el <- as.character(endloccol[i])
    }
    end_location <- c(end_location, el)
  }
  return(end_location)
}

hallsnip_fix <- function(hallsnipcol){
  hallcol <- c()
  for (i in 1:length(hallsnipcol)){
    if (as.character(hallsnipcol[i]) == "V2_V3" & !is.na(hallsnipcol[i])){
      el <- "V2_Y3"
    } else if (as.character(hallsnipcol[i]) == "V2_V4" & !is.na(hallsnipcol[i])){
      el <- "Y2_Y4"
    } else if (as.character(hallsnipcol[i]) == "V4_V1" & !is.na(hallsnipcol[i])){
      el <- "Y4_V1"
    } else {
      el <- as.character(hallsnipcol[i])
    }
    hallcol <- c(hallcol, el)
  }
  return(hallcol)
}


two <- two %>% mutate(end_location = convert_to_pos(ImageFile))
two <- two %>% mutate(hallsnip = convert_to_hall(VideoFile))
two <- two %>% mutate(movement = convert_to_movement(hallsnip))

two <- two %>% mutate(face_dir = convert_to_dir(end_location))
two <- two %>% mutate(sphere = grepl("sphere", two$end_location))

two <- two %>% mutate(end_location = endloc_fix(end_location))
two <- two %>% mutate(hallsnip = hallsnip_fix(hallsnip))

two <- two %>% mutate(letter_loc = convert_to_letter(end_location))

mcoords <- read.csv("location.csv")
colnames(mcoords)[1] <- "letter_loc"

two <- join(two, mcoords, by = "letter_loc")