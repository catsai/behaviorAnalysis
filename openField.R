# Clean the raw data
cleanOpenField <- function(rawMat, endTime) {
  cleaned <- rawMat[c(1,2,4,5,7,11:20)]
  cleaned <- cleaned[cleaned$start < endTime, ]
  cleaned$uniqueName <- paste(cleaned$sn, cleaned$animal, sep = "_")
  cleaned <- data.frame(arrange(as_tibble(cleaned), sn, animal, an))
  return(cleaned)
}

analyzeOpenField <- function(dataMat, expTime) {
  dataList <- list()

  # Clean the raw data
  endTime <- min(max(dataMat$end), expTime*60)
  endCol <- endTime/60+1
  dataList$clean <- cleanOpenField(dataMat, endTime)

  # Separate out into surround or center space
  surround <- dataList$clean[dataList$clean$an == 1, c(16, 5:15)]
  center <- dataList$clean[dataList$clean$an == 2, c(16, 5:15)]

  #Analyze center activity as duration in center or number of entries into center. Also calculate cumulative values.
  centerList <- list()
  #calculate center duration as low and high activity time in center, reshape into wide format
  durationMat <- data.frame(uniqueName = center$uniqueName, start = center$start,
                            duration = center$inadur + center$smldur + center$lardur)
  durationMat <- reshape(durationMat, idvar = "uniqueName", timevar = "start", direction = "wide")
  rownames(durationMat) <- durationMat$uniqueName
  durationMat <- durationMat[,2:endCol]
  centerList$duration <- durationMat
  #reshape center entries into wide format
  enterCountMat <- reshape(center[,1:3], idvar = "uniqueName", timevar = "start", direction = "wide")
  rownames(enterCountMat) <- enterCountMat$uniqueName
  enterCountMat <- enterCountMat[,2:endCol]
  centerList$count <- enterCountMat
  #calculate cumulative values for duration and entries
  centerList$durCumulative <- as.data.frame(t(apply(centerList$duration, 1, cumsum)))
  centerList$countCumulative <- as.data.frame(t(apply(centerList$count, 1, cumsum)))

  #combine locomtion info from both outside area and center
  locomMat <- cbind(uniqueName = surround$uniqueName, start = surround$start,
                    surround[,c(8,9,11,12)] + center[,c(8,9,11,12)])
  #separate out individual variables - small distance, small duration, large distance, large duration
  smallDur <- reshape(locomMat[,c(1,2,3)], idvar = "uniqueName", timevar = "start", direction = "wide")
  smallDist <- reshape(locomMat[,c(1,2,4)], idvar = "uniqueName", timevar = "start", direction = "wide")
  largeDur <- reshape(locomMat[,c(1,2,5)], idvar = "uniqueName", timevar = "start", direction = "wide")
  largeDist <- reshape(locomMat[,c(1,2,6)], idvar = "uniqueName", timevar = "start", direction = "wide")
  #Summarize locomotion values
  locomotionSummary <- data.frame(totalDistance = rowSums(smallDist[,2:endCol]) + rowSums(largeDist[,2:endCol]),
                                  highSpeedTime = rowSums(largeDur[,2:endCol]),
                                  lowSpeedTime = rowSums(smallDur[,2:endCol]),
                                  row.names = smallDur[,1],
                                  stringsAsFactors = F)
  analyzed <- cbind(genotype = rep("NA", nrow(locomotionSummary)), locomotionSummary,
                    centerList$durCumulative, centerList$countCumulative)
  uniqueNameBreaks <- t(data.frame(strsplit(rownames(analyzed), split = "_animal ")))
  analyzed<- cbind(analyzed, session = uniqueNameBreaks[,1], arena = uniqueNameBreaks[,2])

  dataList$analyzed <- analyzed
  

  return(dataList)
}
