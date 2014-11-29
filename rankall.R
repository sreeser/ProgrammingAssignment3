rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  ## Read outcome data
  # "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
  # "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  # "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  state_result = data.frame()
  
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <-outcomeData[,c(2,7,11,17,23)]
  names(data)[1]<-"hospital"
  names(data)[2]<-"usstate"
  names(data)[3]<-"heart attack"
  names(data)[4]<-"heart failure"
  names(data)[5]<-"pneumonia"
  
  #setup vectors of accepted values
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  states <- sort(data[!duplicated(data['usstate']),"usstate"])
  
  
  #couple checks for valid input data
  if(!(outcome %in% valid_outcomes)) stop("invalid outcome") 
  #if(!(state %in% valid_states)) stop("invalid state")  
  
  #suppressing the coersion warnings
  data[,outcome]<-suppressWarnings(as.numeric(data[,outcome]))
  
  for(state in states){
    hospital_rank<-num
    state_data <- subset(data, usstate==state & !is.na(data[,outcome]),select=c("hospital",outcome))
    state_data <-state_data[order(state_data[,outcome],state_data[,"hospital"]),]
    state_data<-cbind(state_data,rank=1:nrow(state_data))
    if(num=="best") hospital_rank<-min(state_data["rank"]) 
    if(num=="worst") hospital_rank<-max(state_data["rank"]) 
    state_result<-rbind(state_result,data.frame(state_data[hospital_rank[1],"hospital"],state))
   
  #set the value for the state and the hospital 
  }
  
  colnames(state_result)<-c("hospital","state")
  
  state_result
}