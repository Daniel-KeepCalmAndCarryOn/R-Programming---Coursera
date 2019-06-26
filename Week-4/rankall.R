rankall <- function(outcome, num = "best"){
    if(!(tolower(outcome) %in% c("heart attack", "heart failure", "pneumonia"))){
        return(message("invalid outcome"))
    }
    
    cap_outcome <- tools::toTitleCase(outcome)
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    state_outcome <- subset(data, select = 
        c(Hospital.Name, 
          State, 
          Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
          Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
          Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    names(state_outcome) <- c("Hospital.Name", "State", "Heart Attack", "Heart Failure", 
                              "Pneumonia")
    sorted_state_outcome <- state_outcome[order(state_outcome$Hospital.Name),]
    
    if(num == "best"){
        all <- data.frame(NULL)
        t <- tapply(sorted_state_outcome[[cap_outcome]], sorted_state_outcome[["State"]], 
                    which.min)
        for(state in names(t)){
            state_best <- subset(sorted_state_outcome, sorted_state_outcome$State == state, 
                                 select = c(Hospital.Name, State))[t[[state]],]
            all <- rbind(all, state_best)
        }
    }
    
    else if(num == "worst"){
        all <- data.frame(NULL)
        t <- tapply(sorted_state_outcome[[cap_outcome]], sorted_state_outcome[["State"]], 
                    which.max)
        for(state in names(t)){
            state_best <- subset(sorted_state_outcome, sorted_state_outcome$State == state, 
                                 select = c(Hospital.Name, State))[t[[state]],]
            all <- rbind(all, state_best)
        }
    }
    
    else{
        all <- data.frame(NULL)
        t <- tapply(as.numeric(sorted_state_outcome[[cap_outcome]]), sorted_state_outcome[["State"]],
                    order)
        for(state in names(t)){
            state_rank_nth <- subset(sorted_state_outcome, sorted_state_outcome$State == state, 
                                     select = c(Hospital.Name, State))[t[[state]][num],]
            all <- rbind(all, state_rank_nth)
        }
    }
    
    names(all) <- c("hospital", "state")
    return(all) 
}