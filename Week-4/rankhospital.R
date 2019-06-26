rankhospital <- function(state, outcome, num = "best"){
    cap_outcome <- tools::toTitleCase(outcome)
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(!(state %in% data$State)){
        return(message("invalid state"))
    }
    if(!(tolower(outcome) %in% c("heart attack", "heart failure", "pneumonia"))){
        return(message("invalid outcome"))
    }
    state_outcome <- subset(data, data$State == state, select = c(Hospital.Name, State, 
        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
        Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    names(state_outcome) <- c("Hospital.Name", "State", "Heart Attack", "Heart Failure", 
                              "Pneumonia")
    sorted_state_outcome <- state_outcome[order(state_outcome$Hospital.Name),]
    if(num > sum(!is.na(sorted_state_outcome[[cap_outcome]]))){
        message("NA")
    }
    if(num == "best"){
        return(sorted_state_outcome[which.min(as.numeric(
            sorted_state_outcome[[cap_outcome]])), 1])
    }
    if(num == "worst"){
        return(sorted_state_outcome[which.max(as.numeric(
            sorted_state_outcome[[cap_outcome]])), 1])
    }
    else{
        sorted_ranked_state_outcome <- sorted_state_outcome[order(as.numeric(
            sorted_state_outcome[[cap_outcome]])),]
        return(sorted_ranked_state_outcome[num, 1])
    }
}