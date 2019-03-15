library(ggplot2)


# given a dataframe, get 
MeanDevsPerTarget <- function(df, targetRepsInBlock){
  meanDevDF <- data.frame(matrix(nrow = nrow(df) / targetRepsInBlock, ncol = ncol(df)))
  colnames(meanDevDF) <- colnames(df)
  
  counter = 1
  for (i in unique(df$ppid)){
    for (j in unique(df$block_num)){
      for (k in unique(df$target_angle)){
        meanDevDF[counter, ] <- df[df$ppid == i & df$block_num == j & df$target_angle == k, ][1,]
        meanDevDF$angular_dev_h[counter] <- mean(df[df$ppid == i & df$block_num == j & df$target_angle == k, 'angular_dev_h'])
        
        counter <- counter + 1
      }
    }
  }
  return(meanDevDF)
}

SubtractBaseline <- function(rotatedSessionDF, alignedSessionDF){
  correctedDF <- rotatedSessionDF
  correctedDF$angular_dev_h <- rotatedSessionDF$angular_dev_h - alignedSessionDF$angular_dev_h
  return(correctedDF)
}






## TESTING
# targetRepsInBlock <- 2
# meanDevNoCurDF <- MeanDevsPerTarget(noCurDF, targetRepsInBlock = targetRepsInBlock)
# 
# 
# 
# listWithPlane <- noCurDF[noCurDF$high_targets == TRUE & noCurDF$rotated_session == TRUE & noCurDF$plane_visible == TRUE, "angular_dev_h"]
# listWithoutPlane <- noCurDF[noCurDF$high_targets == TRUE & noCurDF$rotated_session == TRUE & noCurDF$plane_visible == FALSE, "angular_dev_h"]
# 
# mean(listWithPlane)
# mean(listWithoutPlane)
# 
# t.test(listWithoutPlane, listWithPlane)