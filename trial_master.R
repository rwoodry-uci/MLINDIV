# Split master file into Explore and Test
# Split test master file by rows where movement = "Select"
# Split Explore files by NAs
# For all these newly split data frames grab the letter_locs and store them in as path vectors in lists 
#         that will serve as individual observations in an eventual new file for trial by trial summary

# master_file <- read.csv("MLINDIV_behavioral_master.csv")

splitrows <- which(is.na(master_file$end_location))
splitrows <- c(splitrows, which(master_file$movement == "Select"))
meta <- split(master_file, cumsum(1:nrow(master_file) %in% (splitrows+1)))

# Meta contains all the different trials within each procedure ran for each participant
paths <- c()
eprocs <- c()
Subject <- c()
Task <- c()
Task_type <- c()
Procedure <- c()
Sample <- c()
objlist <- c()
PairList <- c()
startPosition <- c()
startFacing <- c()
EndIm <- c()
StartIm <- c()
Vid_paths <- c()
select_location <- c()
end_rotation <- c()
accuracy <- c()
nodesturns_count <- c()
nodes_count <- c()
average_RT <- c()
trial_starttime <- c()
trial_endtime <- c()
trial_duration <- c()
turns_count <- c()

for (i in 1:length(meta)){
  
  #Path Hallsnip Vectors
  pvid <- meta[[i]]["hallsnip"][[1]]
  pvid <- paste0(pvid[!is.na(pvid)], collapse=" ")
  Vid_paths <- c(Vid_paths, pvid)
  
  # Path vectors
  path <- meta[[i]]["letter_loc"][[1]]
  path <- paste0(path[!is.na(path)], collapse=" ")
  path <- paste(substr(pvid, 1, 1), path)
  paths <- c(paths, path)
  
  # Eprime Procedure file name
  eproc_name <- as.character(meta[[i]]["Eprime.Basename"][[1]][1])
  eprocs <- c(eprocs, eproc_name)
  
  # Subject number
  subject <- as.character(meta[[i]]["Subject"][[1]][1])
  Subject <- c(Subject, subject)
  
  # Task
  task <- as.character(meta[[i]]["Task"][[1]][1])
  Task <- c(Task, task)
  
  # Task_type
  task_type <- as.character(meta[[i]]["Task_type"][[1]][1])
  Task_type <- c(Task_type, task_type)
  
  # Procedure
  proc <- as.character(meta[[i]]["Procedure"][[1]][1])
  Procedure <- c(Procedure, proc)
  
  # Sample
  sample <- as.character(meta[[i]]["Sample"][[1]][1])
  Sample <- c(Sample, sample)
  
  # objlist
  obj <- as.character(meta[[i]]["objlist"][[1]][1])
  objlist <- c(objlist, obj)
  
  # pairlist
  pair <- as.character(meta[[i]]["pairlist"][[1]][1])
  PairList <- c(PairList, pair)
  
  # startPosition
  sp <- as.character(meta[[i]]["startPosition"][[1]][1])
  startPosition <- c(startPosition, sp)
  
  # startFacing
  sf <- as.character(meta[[i]]["startFacing"][[1]][1])
  startFacing <- c(startFacing, sf)
  
  # StartIm
  si <- substr(as.character(meta[[i]]["StartIm"][[1]][1]), 8, 8)
  if (si == "V" & !is.na(si)){ si <- "Y"}
  StartIm <- c(StartIm, si)
  
  # EndIm
  ei <- substr(as.character(meta[[i]]["EndIm"][[1]][1]), 8, 8)
  if(ei == "V" & !is.na(ei)){ei <- "Y"}
  EndIm <- c(EndIm, ei)
  
  # end_location
  nc <- nchar(path)
  last <- substr(path, nc, nc)
  select_location <- c(select_location, last)
  
  # end_rotation
  er <- meta[[i]]["face_dir"][[1]]
  er <- tail(er[!is.na(er)], 1)
  end_rotation <- c(end_rotation, er)
  
  # v-----Custom Calculations-----v
  
  # Accuracy
  ac <- last == ei
  accuracy <- c(accuracy, ac)
  
  # Nodes Turns Count
  nodes <- strsplit(path, " ")[[1]]
  ncount <- length(nodes)
  nodesturns_count <- c(nodesturns_count, ncount)
  
  # Nodes Count (no turns)
  vp <- strsplit(pvid, " ")[[1]]
  vp <- length(vp[substr(vp, 1, 1) != substr(vp, 4, 4)])
  nodes_count <- c(nodes_count, vp)
  
  # Turn Count
  tc <- ncount- vp
  turns_count <- c(turns_count, tc)

  # Average Response Time
  avRT <- mean(meta[[i]]["Choose.RT"][[1]], na.rm=TRUE)
  average_RT <- c(average_RT, avRT)
  
  # Trial Completion Duration
  starttime <- meta[[i]]["Choose.OnsetTime"][[1]][1]
  endtime <- meta[[i]]["Choose.RTTime"][[1]]
  endtime <- tail(endtime[!is.na(endtime)], 1)
  tctime <- endtime - starttime
  trial_duration <- c(trial_duration, tctime)
  
  # Trial Start Time
  trial_starttime <- c(trial_starttime, starttime)
  
  # Trial Completion Time
  trial_endtime <- c(trial_endtime, endtime)
  
  # Trial Difficulty/Length
  # Shortest Path
  # Path Length
  
}

trial_master <- data.frame(Subject = Subject, eprocs = eprocs, Task_ype = Task_type, Procedure = Procedure,
                           Sample = Sample, objlist = objlist, PairList = PairList, startPosition = startPosition, 
                           startFacing = startFacing, paths = paths, Vid_paths = Vid_paths, StartIm = StartIm, EndIm = EndIm, 
                           end_location = select_location, end_rotation = end_rotation, accuracy = accuracy, 
                           nodesturns_count = nodesturns_count, nodes_count = nodes_count, turns_count = turns_count, average_RT = average_RT,
                           trial_starttime = trial_starttime, trial_endtime = trial_endtime, trial_duration = trial_duration)

dtable <- read.csv("distancetable.csv")
dtable[30,1] <- "P"
dtable[44, 1] <- "P"
colnames(dtable)[1] <- "StartIm"
colnames(dtable)[2] <- "EndIm"
t <- join(trial_master, dtable, by = c("StartIm", "EndIm"))
