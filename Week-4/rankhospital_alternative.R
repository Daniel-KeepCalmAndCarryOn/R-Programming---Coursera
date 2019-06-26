rankhospital <- function(state, outcome, num = "best"){
    ## Refer to best.R for comments for code from line 3 to line 20
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(!(state %in% data$State)){
        stop("invalid state")
    }
    if(!(tolower(outcome) %in% c("heart attack", "heart failure", "pneumonia"))){
        stop("invalid outcome")
    }
    
    cap_outcome <- tools::toTitleCase(outcome)
    state_outcome <- subset(data, data$State == state, select = 
                                c(Hospital.Name, 
                                  State, 
                                  Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                  Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                  Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    names(state_outcome) <- c("Hospital.Name", "State", "Heart Attack", "Heart Failure", 
                              "Pneumonia")
    alpha_state_outcome <- state_outcome[order(state_outcome$Hospital.Name),]
    
    ## Convert the class of character to numeric
    alpha_state_outcome[[cap_outcome]] <-
        suppressWarnings(as.numeric(alpha_state_outcome[[cap_outcome]]))
    
    ## Sort by the outcome, meanwhile drop NA values
    ranked_alpha_state_outcome <- 
        alpha_state_outcome[order(alpha_state_outcome[[cap_outcome]], na.last = NA),]
    
    if(num == "best"){rank_num <- 1}
    else if(num == "worst"){rank_num <- nrow(ranked_alpha_state_outcome)}
    
    ## Use return() to exit the function
    else if(num > nrow(ranked_alpha_state_outcome)){return(message("NA"))}
    else{rank_num <- num} 
    
    return(ranked_alpha_state_outcome$Hospital.Name[rank_num])
}