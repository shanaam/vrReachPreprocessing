library(ggplot2)

# Not sure about processing time in R vs Python

# some constants:
homePositionLoc <- list('x' = 0, 'y' = 0.9, 'z' = 0.2)
numOfTrials <- 321 # total # of trials
planeVisibleBlocksA <- c(4, 8, 12, 16)
planeVisibleBlocksB <- c(2, 6, 10, 14)
highTargetBlocksA <- c(6, 8, 14, 16)
highTargetBlocksB <- c(6, 8, 14, 16)

# given a row of data (as a df), returns a vector with x,y,z positions that are adjusted for hompePosition
PositionVec <- function(dfRow)
  return (c(dfRow$pos_x - homePositionLoc$x, dfRow$pos_y - homePositionLoc$y, dfRow$pos_z - homePositionLoc$z))


# given a vector, find the euclidian normal (magnitude)
NormVec <- function(vec) sqrt(sum(vec^2))


# given a row, find the distance from home position
DistanceFromHome <- function (dfRow){
  
  # first, subtract the home position to get a vector relative to the origin
  locationVector <- PositionVec(dfRow)
  
  # ONLY USE X + Z plane
  locationVector <- c(locationVector[1], locationVector[3])
  
  # find the length (magnitude, or euclidian norm) of the vector
  distance <- NormVec(locationVector)
  
  return(distance)
}


# given a 3-D vector, find spherical (magnitude, theta, phi)
SphericalVec <- function(vec){
  magnitude <- NormVec(vec)
  
  # theta <- acos(vec[1] / magnitude) * 180/pi                # the 180/pi is to turn radians to degrees
  theta <- (atan2(vec[3], vec[1]) * 180/pi) %% 360             # abs is a bad way to do it because sometimes, the direction is negative... This is a problem with the coordinate frames unity uses vs what I am expecting. Work out the math on paper
  
  # phi <- acos(vec[1] / NormVec(vec[1:2])) * 180/pi
  phi <- acos(vec[2] / magnitude) * 180/pi
  
  return(c(magnitude, theta, phi))
}


# given a string (path to a directory containing all trials), generate a dataframe of trial by trial angles
MakeByTrialDF <- function(path) {
  # instantiate a dataframe to eventualy fill
  byTrialDF <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(byTrialDF) <- c('trial_num', 'magnitude', 'theta', 'phi')
  
  # get a list of CSVs containing reach trials in this directory
  listOfFiles <- list.files(path = path)
  listOfFiles <- grep("example_object_movement_", listOfFiles, value = TRUE)
  
  # use this to cycle through files (to build up the byTrialDF)
  for (csvName in listOfFiles){
    # generate path to this particular csv
    pathToCSV <- paste(path, csvName, sep = '')
    
    # read the csv
    rawReachDF <- read.csv(pathToCSV, header = TRUE)
    
    # get the row at 3cm
    for(i in 1:nrow(rawReachDF)) {
      row <- rawReachDF[i,]

      # do stuff with row
      # if distance of the row (minus home) is greather than 3cm...
      if (DistanceFromHome(row) >= 0.03){
        # get position vector from the row
        positionVec <- PositionVec(row)
        
        # get the spherical of the vector
        trialSpherical <- SphericalVec(positionVec)
        
        # this trial's trial_num
        trialNum <- as.numeric(substr(csvName, nchar(csvName) - 6, nchar(csvName) - 4))
        
        # combine those 2
        dataToAdd <- c(trialNum, trialSpherical)
        
        # add the new data to our DF
        byTrialDF[nrow(byTrialDF) + 1,] <- dataToAdd
        
        # break out of this loop!
        break
      }
    }
  }
  
  pathTobyTrial <- paste(path, 'trial_results.csv', sep = '')
  byTrialInfo <- read.csv(pathTobyTrial)
  
  # flip target angle in by trial csv around 90 *** use apply here?
  byTrialInfo$target_angle <- (((byTrialInfo$target_angle - 90) * -1) + 90)
  
  byTrialDF$theta <- byTrialDF$theta - byTrialInfo$target_angle
  
  return(byTrialDF)
}

# given a row in trial_results.csv return a single theta value
ThetaFromRow <- function(directory, example_object_movement_filename){
  pathToReach <- paste("data/raw", directory, example_object_movement_filename, sep = "/")
  
  reachCSV <- read.csv(pathToReach, header = TRUE)
  
  # get the row at 3cm
  for(i in 1:nrow(reachCSV)) {
    row <- reachCSV[i,]
    
    # do stuff with row
    # if distance of the row (minus home) is greather than 3cm...
    if (DistanceFromHome(row) >= 0.03){
      # get position vector from the row
      positionVec <- PositionVec(row)
      
      # get the spherical of the vector
      trialSpherical <- SphericalVec(positionVec)
      
      return(trialSpherical[2])
    }
  }
}


### FUNCTIONS FOR MAKING CSVS

# fill up the per_ppt directory with angular deviations per trial
MakePerPptCSVs <- function(rawDataPath){
  # build a list of experiment versions/types
  expTypeList <- list.files(path = rawDataPath)
  
  for (expType in expTypeList){
    expTypePath <- paste(rawDataPath, expType, sep = '')
    
    # for each exp type; build a participant list
    pptList <- list.files(path <- expTypePath)
    
    for (ppt in pptList){
      # for each ppt in the expType, make path to that directory
      path <- paste(expTypePath,ppt,"S001/", sep = "/")
      
      # make the byTrialDF
      byTrialDF <- MakeByTrialDF(path)
      
      # save each byTrialDF in the data/per_ppt directory
      pathToCSVSave <- paste('data/per_ppt/', ppt, '.csv', sep = '')
      
      write.csv(byTrialDF, file = pathToCSVSave)
    }
  }
}

# given per_ppt directory path make a byTrialThetas csv 
MakeByTrialThetas <- function(perPptPath){
  
  # make a master dataframe to populate
  masterDF <- data.frame(matrix(ncol = 0, nrow = numOfTrials))
  exampleInfoDF <- read.csv('data/raw/visible_plane - A/1/S001/trial_results.csv', header = TRUE)
  
  masterDF$trial_num <- exampleInfoDF$trial_num
  masterDF$block_num <- exampleInfoDF$block_num
  masterDF$trial_num_in_block <- exampleInfoDF$trial_num_in_block
  masterDF$cursor_visibility <- exampleInfoDF$cursor_visibility
  masterDF$trial_type <- exampleInfoDF$trial_type
  masterDF$rotation <- exampleInfoDF$rotation
  
  # get a list of per_ppt csvs
  listOfCSVs <- list.files(perPptPath)
  for (csv in listOfCSVs){
    pathToCSV <- paste(perPptPath, csv, sep = '/')
    
    perPptCSV <- read.csv(pathToCSV, header = TRUE)
    
    masterDF <- cbind(masterDF, perPptCSV$theta)
    colnames(masterDF)[length(colnames(masterDF))] <- substr(csv, 1, 1)
  }
  
  pathToCSVSave <- 'data/processed/byTrialThetas.csv'
  
  # write the masterCSV to file
  write.csv(masterDF, pathToCSVSave)
}


MakeBetterByTrialCSVs <- function(rawDataPath){
  # build a list of experiment versions/types
  expTypeList <- list.files(path = rawDataPath)
  
  for (expType in expTypeList){
    expTypePath <- paste(rawDataPath, expType, sep = '')
    
    # for each exp type; build a participant list
    pptList <- list.files(path <- expTypePath)
    
    for (ppt in pptList){
      # for each ppt in the expType, make path to that directory
      path <- paste(expTypePath,ppt,"S001","trial_results.csv", sep = "/")
      
      byTrialInfo <- read.csv(path, header = TRUE)
      # fix the reach angles in byTrialInfo
      byTrialInfo$target_angle <- (((byTrialInfo$target_angle - 90) * -1) + 90)
      
      # calculate thetas
      angular_dev_h <- mapply(ThetaFromRow, byTrialInfo$directory, byTrialInfo$example_object_movement_filename)
      
      # subtract out the target angles
      angular_dev_h <- angular_dev_h - byTrialInfo$target_angle
      
      byTrialInfo$angular_dev_h <- angular_dev_h
      
      
      # add plane visibility column
      # add target height column
      byTrialInfo$plane_visible <- FALSE
      byTrialInfo$high_targets <- FALSE
      if(grepl('- A', byTrialInfo$experiment[1])){
        byTrialInfo$plane_visible[byTrialInfo$block_num %in% planeVisibleBlocksA] <- TRUE
        byTrialInfo$high_targets[byTrialInfo$block_num %in% highTargetBlocksA] <- TRUE
      }
      else if (grepl('- B', byTrialInfo$experiment[1])){
        byTrialInfo$plane_visible[byTrialInfo$block_num %in% planeVisibleBlocksB] <- TRUE
        byTrialInfo$high_targets[byTrialInfo$block_num %in% highTargetBlocksB] <- TRUE
      }
      
      savePath <- paste("data/processed/per_ppt/", ppt, ".csv", sep = '')
      write.csv(byTrialInfo, savePath)
    }
  }
}


# using the improved by trial csvs, stitch together ONLY the no-cursor data
MakeNocurCSV <- function(perPptPath, numCols, numNoCurTrials){
  # get a list of all the files in the directory
  pptCSVList <- list.files(perPptPath)
  
  # make an empty dataframe to populate
  noCurDF <- data.frame(matrix(ncol = numCols, nrow = numNoCurTrials*length(pptCSVList)))
  
  for (i in seq_along(pptCSVList)){
    pptCSVPath <- paste(perPptPath,pptCSVList[i], sep = "")
    
    pptDF <- read.csv(pptCSVPath, header = TRUE)
    noCurRows <- pptDF[pptDF$cursor_visibility == FALSE | pptDF$cursor_visibility == 'False', ]
    
    for (j in seq(nrow(noCurRows))){
      noCurDF[((i-1) * 96) + j, ] <- noCurRows[j, ]
    }
    
    # noCurDF <- rbind(noCurDF, noCurRows) # THIS IS SLOW!
  }
  
  #set column names
  colnames(noCurDF) <- colnames(pptDF)
  
  #remove useless columns
  noCurDF$X <- NULL
  noCurDF$directory <- NULL
  noCurDF$experiment <- NULL
  noCurDF$test_to_log <- NULL
  noCurDF$cursor_visibility <- NULL
  noCurDF$rotation <- NULL
  noCurDF$trial_type <- NULL
  noCurDF$example_object_movement_filename <- NULL
  noCurDF$session_num <- NULL
  
  # add a rotated session column
  noCurDF$rotated_session <- FALSE
  noCurDF$rotated_session[noCurDF$block_num > 8] <- TRUE
  
  
  
  # save the noCursorDF
  savePath <- 'data/processed/noCursorsOnly.csv'
  write.csv(noCurDF, savePath)
}



# TESTING
# rawDataPath <- 'data/raw/'
# MakeBetterByTrialCSVs(rawDataPath)

## MakePerPptCSVs(rawDataPath)

perPptPath <- 'data/processed/per_ppt/'
MakeNocurCSV(perPptPath, numCols = 19, numNoCurTrials = 96)




### 

testPath <- 'data/raw/visible_plane - B/6/S001/'
testByTrialDF <- MakeByTrialDF(testPath)

p <- ggplot(testByTrialDF) +
  geom_point(aes(trial_num, theta, colour = "theta"))
  # geom_point(aes(trial_num, phi))
p

# NOTE: phi is very much correlated with theta. Plot some of these in 3-D and see what's up
# load file
rawReachDF <- read.csv('data/raw/visible_plane - B/4/S001/example_object_movement_T317.csv', header = TRUE)
# plot a reach (just for visual)
p <- ggplot(rawReachDF, aes(pos_x, pos_z)) +
  geom_point() +
  scale_x_continuous(limits = c(-0.10, 0.10)) +
  scale_y_continuous(limits = c(0.15, 0.35))
p

row <- rawReachDF[]


