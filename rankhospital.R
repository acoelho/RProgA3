rankhospital <- function(state, outcome, num = "best") {
  ##Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  ##Check that state and outcome are valid
  if(!is.element(state,data$State)) stop("invalid state")
  if(outcome != "heart failure" & outcome != "heart attack" & outcome != "pneumonia") stop("invalid outcome")
  
  state_data <- data[data$State == state,]
  if(outcome == "heart attack") column <- 11
  if(outcome == "heart failure") column <- 17
  if(outcome == "pneumonia") column <- 23
  

  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  mrates <- state_data[!state_data[,column] == "Not Available",]
  
  ranked <- mrates[order(mrates[,column]),]
  print(ranked[,2])
  if(num == "best")return(as.character(head(ranked[,2],1)))
  if(num == "worst") return(as.character(tail(ranked[,2],1)))
  return (as.character(ranked[num,2]))
}  
