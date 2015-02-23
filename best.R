best <- function(state, outcome) {
  ##Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  ##Check that state and outcome are valid
  if(!is.element(state,data$State)) stop("invalid state")
  if(outcome != "heart failure" & outcome != "heart attack" & outcome != "pneumonia") stop("invalid outcome")
  ##Return hospital name in that state with lowest 30-day death rate
  state_data <- data[data$State == state,]
  if(outcome == "heart attack") column <- 11
  if(outcome == "heart failure") column <- 17
  if(outcome == "pneumonia") column <- 23

  
  mrates <- state_data[!state_data[,column] == "Not Available",column]
    
  min <- min(as.numeric(as.character(mrates)))
  
  
  hosp <- state_data[state_data[,column] == min,2]
  return(as.character(hosp[1]))
}  
  