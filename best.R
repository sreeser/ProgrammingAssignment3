best <- function(state, outcome) {
  ## Read outcome data
  # "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
  # "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  # "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <-outcomeData[,c(2,7,11,17,23)]
  names(data)[1]<-"hospital"
  names(data)[2]<-"usstate"
  names(data)[3]<-"heart attack"
  names(data)[4]<-"heart failure"
  names(data)[5]<-"pneumonia"
  
  #setup vectors of accepted values
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  valid_states <- data[!duplicated(data['usstate']),"usstate"]
  
  #couple checks for valid input data
  if(!(outcome %in% valid_outcomes)) stop("invalid outcome") 
  if(!(state %in% valid_states)) stop("invalid state")  
 
  #not clear if complete cases just in the outcome or across all outcomes
  #suppressing the coersion warnings
  data[,outcome]<-suppressWarnings(as.numeric(data[,outcome]))
 
  #only those for whom we have a value for the state indicated
  data <- subset(data, usstate==state & !is.na(data[,outcome]),select=c("hospital",outcome))
  
  #sort by rate then name
  data <-data[order(data[,outcome],data[,"hospital"]),]
  
  #return the name of the hospital with lowest rate
  data[1,"hospital"]
  }