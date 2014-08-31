best <- function(state, outcome) {
  library(data.table)
  ## Read outcome data
  outcomeFile <- data.table(read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = "Not Available"))
  
  #outcomeFile <- na.omit(outcomeFile)
  POSIBLE_OUTCOME = c("heart attack", "heart failure", "pneumonia")
  
  
  ## Check that state and outcome are valid
  if (!(state %in% outcomeFile$State))
    stop("invalid state")
  
  if (!(outcome %in% POSIBLE_OUTCOME))
    stop("invalid outcome")
  
  if (outcome == POSIBLE_OUTCOME[1]) {
    index = 11
  } else {
    
    if (outcome == POSIBLE_OUTCOME[2]) {
    index = 17
    } else {
      index = 23
    }
  }
  
  setkey(outcomeFile, State)
  temp1 <- data.table(outcomeFile[state])
  
  temp1 <- temp1[,c(3,index), with=FALSE]
  setnames(temp1, c("hospital", "mortalityRate"))
  temp1 = temp1[, mortalityRate:=as.numeric(mortalityRate)]
  setkey(temp1, mortalityRate)
  temp1 <- na.omit(temp1)
  
  min <- min(temp1[,as.numeric(c(mortalityRate))])
  
  temp1 = temp1[J(min)]
  temp1 = temp1[order(hospital)]
  
  
  return(temp1[1, c(hospital)])
  
    
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}