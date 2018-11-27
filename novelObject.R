# Clean data by removing extra columns and erroneous rows
clean <- function(rawMat, endTime) {
  cleaned <- rawMat[,c(1,2,4,13:20)]
  cleaned <- cleaned[cleaned$start < endTime, ]
  cleaned$uniqueName <- paste(cleaned$sn, cleaned$animal, sep = "_")
  cleaned <- data.frame(arrange(as_tibble(cleaned), sn, animal, an))
  return(cleaned)
}

#Summarize locomotion data as total distance (smldist+lardist), high velocity time (lardur), and low velocity time (smldur)
summarizeActivity <- function(arenaMat, endCol) {
  #separate out individual variables - small distance, small duration, large distance, large duration
  smallDur <- reshape(arenaMat[,c(1,5,6)], idvar = "uniqueName", timevar = "start", direction = "wide")
  smallDist <- reshape(arenaMat[,c(1,5,7)], idvar = "uniqueName", timevar = "start", direction = "wide")
  largeDur <- reshape(arenaMat[,c(1,5,8)], idvar = "uniqueName", timevar = "start", direction = "wide")
  largeDist <- reshape(arenaMat[,c(1,5,9)], idvar = "uniqueName", timevar = "start", direction = "wide")

  #Summarize locomotion values
  locomotionSummary <- data.frame(totalDistance = rowSums(smallDist[,2:endCol]) + rowSums(largeDist[,2:endCol]),
                                  highSpeedTime = rowSums(largeDur[,2:endCol]),
                                  lowSpeedTime = rowSums(smallDur[,2:endCol]),
                                  row.names = smallDur[,1],
                                  stringsAsFactors = F)
  return(locomotionSummary)
}

#Reformat shape of data into wide format of individual variables for objects (counts/object and duration/object).
# Calculate cumulative values over time for each variable.
summarizeObjData <- function(arenaMat, endCol) {
  summaryList <- list()
  #separate out individual variables - count and duration
  summaryList$count <- reshape(arenaMat[,c(1,5,6)], idvar = "uniqueName", timevar = "start", direction = "wide")
  rownames(summaryList$count) <- summaryList$count[,1]
  summaryList$count <- summaryList$count[2:endCol]
  summaryList$duration <- reshape(arenaMat[,c(1,5,7)], idvar = "uniqueName", timevar = "start", direction = "wide")
  rownames(summaryList$duration) <- summaryList$duration[,1]
  summaryList$duration <- summaryList$duration[2:endCol]
  #Calculate cumulative values for count and duration
  summaryList$countCumulative <- as.data.frame(t(apply(summaryList$count, 1, cumsum)))
  rownames(summaryList$countCumulative) <- rownames(summaryList$count)
  summaryList$durationCumulative <- as.data.frame(t(apply(summaryList$duration, 1, cumsum)))
  rownames(summaryList$durationCumulative) <- rownames(summaryList$duration)
  return(summaryList)
}

separateByArena <- function(cleanMat, expType, endCol) {
  arenaList <- list()
  arenaList$field <- summarizeActivity(cleanMat[cleanMat$an == 0, c(12, 2:5, 8:11)], endCol)
  arenaList$obj1 <- summarizeObjData(cleanMat[cleanMat$an == 2, c(12, 2:7)], endCol)
  arenaList$obj2 <- summarizeObjData(cleanMat[cleanMat$an == 3, c(12, 2:7)], endCol)
  if(expType == "Novel Object Location") {
    arenaList$obj3 <- summarizeObjData(cleanMat[cleanMat$an == 4, c(12, 2:7)], endCol)
  }
  return(arenaList)
}

#Calculate preference
preferenceRatio <- function(arenaInfo, expType, endCol) {
  totalCount <- arenaInfo$obj1$countCumulative + arenaInfo$obj2$countCumulative
  totalExplore <- arenaInfo$obj1$durationCumulative + arenaInfo$obj2$durationCumulative
  pref <- round((arenaInfo$obj2$durationCumulative/totalExplore)*100, 3)
  if(expType == "Novel Object Location"){
    totalCount <- arenaInfo$obj1$countCumulative + arenaInfo$obj2$countCumulative + arenaInfo$obj3$countCumulative
    avgOldObj <- as.data.frame(t(apply(((arenaInfo$obj1$duration + arenaInfo$obj2$duration)/2), 1, cumsum)))
    pref <- round((arenaInfo$obj3$durationCumulative/(arenaInfo$obj3$durationCumulative + avgOldObj)),3)*100
  }
  pref <- cbind(totalExplore[,endCol], totalCount[,endCol], pref)
  return(pref)
}

analyzeNovelObject <- function(trainMat, testMat, expType, expTime) {
  dataList <- list()
  
  # Clean data for training and testing days
  endTrainCol <- as.numeric(strsplit(trainMat$expdur[1], ":")[[1]][2])
  endTrain <- endTrainCol*60
  endTestCol <- as.numeric(strsplit(testMat$expdur[1], ":")[[1]][2])
  endTest <- endTestCol*60
  
  dataList$cleanTrain <- clean(trainMat, endTrain)
  dataList$cleanTest <- clean(testMat, endTest)
  
  # Separate out into surrounding field, old object, and new object arenas for training and testing days
  arenasTrain <- separateByArena(dataList$cleanTrain, expType, endTrainCol+1)
  arenasTest <- separateByArena(dataList$cleanTest, expType, endTestCol+1)
  
  # Calculate preference ratio (Time_new)/(Time_new + Time_old) for training and testing days
  prefTrain <- preferenceRatio(arenasTrain, expType, endTrainCol)
  prefTest <- preferenceRatio(arenasTest, expType, endTestCol)
  
  # Summarize training and testing days
  colTrainNames <- c("Total.Distance", "High.Speed.Time", "Low.Speed.Time", "Exploration.Time", "Approach.Count",
                     paste("Pref", seq(60, endTrain, 60), sep = "."))
  colTestNames <- c("Total.Distance", "High.Speed.Time", "Low.Speed.Time", "Exploration.Time", "Approach.Count",
                    paste("Pref", seq(60, endTest, 60), sep = "."))
  trainSummary <- cbind(arenasTrain$field, prefTrain)
  testSummary <- cbind(arenasTest$field, prefTest)
  colnames(trainSummary) <- colTrainNames
  colnames(testSummary) <- colTestNames
  analysisSummary <- cbind(genotype = rep("NA", nrow(trainSummary)), trainSummary[,c(1,4,5,endTrainCol+5)],
                           testSummary)
  rNames <- rownames(analysisSummary)
  uniqueNameBreaks <- t(data.frame(strsplit(rNames, split = "_animal ")))
  analysisSummary <- cbind(session = uniqueNameBreaks[,1], arena = uniqueNameBreaks[,2],analysisSummary)
  rownames(analysisSummary) <- rNames
  
  dataList$trainSummary <- trainSummary
  dataList$testSummary <- testSummary
  dataList$analyzed <- analysisSummary
  
  return(dataList)
}
