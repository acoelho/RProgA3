rankall <- function(outcome, num = "best") {
  ##Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  ##Check that state and outcome are valid
  ##if(!is.element(state,data$State)) stop("invalid state")
  if(outcome != "heart failure" & outcome != "heart attack" & outcome != "pneumonia") stop("invalid outcome")
  

  if(outcome == "heart attack") column <- 11
  if(outcome == "heart failure") column <- 17
  if(outcome == "pneumonia") column <- 23
  
  
  ## For each state, find the hospital of the given rank
  states <- sort(unique(data$State))
  hospital <- c()
    
  for(i in 1:length(states)) {
    state <- states[i]
    state_data <- data[data$State == state,]
    mrates <- state_data[!state_data[,column] == "Not Available",]
    ranked <- mrates[order(as.numeric(as.character(mrates[,column])),as.character(mrates[,2])),]

    if(num == "best") {
      hospital[i] <- as.character(head(ranked[,2],1))
    }
    else if(num == "worst") {
      hospital[i] <- as.character(tail(ranked[,2],1))
    }
    else hospital[i] <- as.character(ranked[num,2])
  }

  results <- data.frame(hospital,states)
  colnames(results)<- c("hospital", "state")
  rownames(results) <-states
  return(results)
 
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  

}  
