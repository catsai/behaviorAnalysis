# Clean data by removing extra columns and erroneous rows
cleanBarnes <- function(rawMat) {
  cleaned <- rawMat[,c(4,5,7,8,11:22)]
  cleaned <- data.frame(arrange(as_tibble(cleaned), sn, start, an))
  cleaned$entct <- ifelse(cleaned$entct >= 1, 1,0)
  return(cleaned)
}

#Summarize locomotion data as total distance, high velocity time, and low velocity time in entire arena
summarizeBarnesActivity <- function(cleanMat) {
  fieldMat <- cleanMat[cleanMat$an == 1, c(7,8,10,11,13,14)]
  locoTemp <- colSums(fieldMat)
  locomotionSummary <- data.frame(Total.Distance = locoTemp["inadist"] + locoTemp["smldist"] + locoTemp["lardist"],
                                  High.Speed = locoTemp["lardur"],
                                  Low.Speed = locoTemp["smldur"],
                                  Total.Time = locoTemp["inadur"] + locoTemp["smldur"] + locoTemp["lardur"])
  return(locomotionSummary)
}

#Summarize primary latency to escape hole
summarizePrimaryInfo <- function(cleanMat, escapeBox, recordedLatency) {
  fieldMat <- cleanMat[cleanMat$an == 1, 2:14]
  arenasMat <- cleanMat[cleanMat$an > 1, 2:14]
  arenasMat$entct <- ifelse(arenasMat$entct >= 1, 1, 0)
  
  targetHoleEntries <- sum(arenasMat$entct[arenasMat$an == escapeBox])
  holeSearchError <- sum(arenasMat$entct[arenasMat$an != escapeBox])
  
  primaryHoleSearch <- sum(arenasMat$entct[arenasMat$an != escapeBox & arenasMat$start <= recordedLatency])
  primaryFieldInfo <- colSums(fieldMat[fieldMat$start <= recordedLatency, c(6,7,9,10,12,13)])
  primaryLatencyWindow <- primaryFieldInfo["inadur"] + primaryFieldInfo["smldur"] + primaryFieldInfo["lardur"]
  primaryDistance <- primaryFieldInfo["inadist"] + primaryFieldInfo["smldist"] + primaryFieldInfo["lardist"]
  
  searchSummary <- data.frame(Target.Hole.Entries = targetHoleEntries, 
                              Hole.Search.Error = holeSearchError,
                              Primary.Hole.Search = primaryHoleSearch, 
                              Recorded.Latency = recordedLatency, Calc.Latency = primaryLatencyWindow, 
                              Primary.Distance = primaryDistance)
  return(searchSummary)
}

#Custom modulo function since escape arenas are placed like a clock face
wrapAround <- function(thisSeq) {
  for(i in 1:length(thisSeq)) {
    if(thisSeq[i] > 21) {
      thisSeq[i] <- (thisSeq[i]%%21 + 1)
    } else if(thisSeq[i] < 2) {
      thisSeq[i] <- (thisSeq[i] + 20)
    }
  }
  return(thisSeq)
}

#Summarize testing results as entries or time in each quadrant
summarizeBarnesTesting <- function(cleanMat, escapeBox) {
  targetHoles <- wrapAround(seq(escapeBox-2,escapeBox+2))
  positiveHoles <- wrapAround(seq(escapeBox+3,escapeBox+7))
  oppositeHoles <- wrapAround(seq(escapeBox+8,escapeBox+12))
  negativeHoles <- wrapAround(seq(escapeBox+13,escapeBox+17))
  
  targetEnt <- sum(cleanMat$entct[cleanMat$an %in% targetHoles])
  positiveEnt <- sum(cleanMat$entct[cleanMat$an %in% positiveHoles])
  oppositeEnt <- sum(cleanMat$entct[cleanMat$an %in% oppositeHoles])
  negativeEnt <- sum(cleanMat$entct[cleanMat$an %in% negativeHoles])
  
  targetDur <- sum(colSums(cleanMat[cleanMat$an == 22, c(7,10,13)]))
  positiveDur <- sum(colSums(cleanMat[cleanMat$an == 23, c(7,10,13)]))
  oppositeDur <- sum(colSums(cleanMat[cleanMat$an == 24, c(7,10,13)]))
  negativeDur <- sum(colSums(cleanMat[cleanMat$an == 25, c(7,10,13)]))
  totalQuadDur <- targetDur + positiveDur + oppositeDur + negativeDur
  
  testingSummary <- data.frame(Target.Entry = targetEnt, Positive.Entry = positiveEnt, 
                               Opposite.Entry = oppositeEnt, Negative.Entry = negativeEnt,
                               Target.Dur = targetDur, Target.Percent = round(targetDur/totalQuadDur,3)*100,
                               Positive.Dur = positiveDur, Positive.Percent = round(positiveDur/totalQuadDur,3)*100,
                               Opposite.Dur = oppositeDur, Opposite.Percent = round(oppositeDur/totalQuadDur,3)*100,
                               Negative.Dur = negativeDur, Negative.Percent = round(negativeDur/totalQuadDur,3)*100)
  return(testingSummary)
}

#Analyze function
analyzeBarnesData <- function(barnesData, dataType, escapeBox, recLatency) {
  cleanedData <- cleanBarnes(barnesData)
  serialNums <- unique(cleanedData$sn)
  
  analysisSummary <- data.frame(Total.Distance = 0, High.Speed = 0, Low.Speed = 0, Total.Time = 0,
                                Target.Hole.Entries = 0, Hole.Search.Error = 0,
                                Primary.Hole.Search = 0, Recorded.Latency = 0, 
                                Calc.Latency = 0, Primary.Distance = 0)
  if(dataType == "test") {
    analysisSummary <- cbind(analysisSummary, data.frame(Target.Entry = 0, Positive.Entry = 0, 
                                                         Opposite.Entry = 0, Negative.Entry = 0,
                                                         Target.Dur = 0, Target.Percent = 0,
                                                         Positive.Dur = 0, Positive.Percent = 0,
                                                         Opposite.Dur = 0, Opposite.Percent = 0,
                                                         Negative.Dur = 0, Negative.Percent = 0))
  }
  rownames(analysisSummary) <- "temp"
  
  for(i in serialNums) {
    currAnimal <- cleanedData[cleanedData$sn == i,]
    currDT <- cbind(summarizeBarnesActivity(currAnimal), summarizePrimaryInfo(currAnimal, escapeBox, recLatency[i]))
    if(dataType == "test") {
      currDT <- cbind(currDT, summarizeBarnesTesting(currAnimal, escapeBox))
    }
    rNames <- c(rownames(analysisSummary), as.character(i))
    analysisSummary <- rbind(analysisSummary,currDT)
    rownames(analysisSummary) <- rNames
  }
  
  analysisSummary <- analysisSummary[-1,]
  dataList <- list(cleanBarnes = cleanedData, analyzedBarnes = analysisSummary)
}



