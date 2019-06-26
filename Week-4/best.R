best <- function(state, outcome){
    if(!(state %in% data$State)){
        return(message("invalid state"))
    }
    if(!(tolower(outcome) %in% c("heart attack", "heart failure", "pneumonia"))){
        return(message("invalid outcome"))
    }
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(tolower(outcome) == "heart attack"){
        state_HA <- subset(data, data$State == state, select = c(Hospital.Name, State, 
            Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
        sorted_state_HA <- state_HA[order(state_HA$Hospital.Name),]
        return(sorted_state_HA[which.min(as.numeric(sorted_state_HA[[3]])), 1])
    }
    if(tolower(outcome) == "heart failure"){
        state_HF <- subset(data, data$State == state, select = c(Hospital.Name, State, 
            Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
        sorted_state_HF <- state_HF[order(state_HF$Hospital.Name),]
        return(sorted_state_HF[which.min(as.numeric(sorted_state_HF[[3]])), 1])
    }
    if(tolower(outcome) == "pneumonia"){
        state_P <- subset(data, data$State == state, select = c(Hospital.Name, State, 
            Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        sorted_state_P <- state_P[order(state_P$Hospital.Name),]
        return(sorted_state_P[which.min(as.numeric(sorted_state_P[[3]])), 1])
    }
}