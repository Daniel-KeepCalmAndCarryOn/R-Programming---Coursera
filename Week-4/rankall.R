rankall <- function(outcome, num = "best"){
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(!(tolower(outcome) %in% c("heart attack", "heart failure", "pneumonia"))){
        stop("invalid outcome")
    }
    
    cap_outcome <- tools::toTitleCase(outcome)
    all_outcome <- subset(data, select = 
                                c(Hospital.Name, 
                                  State, 
                                  Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                  Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                  Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    names(all_outcome) <- c("Hospital.Name", "State", "Heart Attack", "Heart Failure", 
                              "Pneumonia")
    alpha_all_outcome <- all_outcome[order(state_outcome$Hospital.Name),]
    
    ## Create an empty data frame to store the hosptial names and state names
    all <- data.frame(NULL)
    
    if(num == "best"){
        ## Use tapply() to apply which.min to the outcome column subset by states
        t <- tapply(alpha_all_outcome[[cap_outcome]], alpha_all_outcome[["State"]], 
                    which.min)
        ## Store the best hospital for each state into the variable "state_best"
        for(state in names(t)){
            state_best <- subset(alpha_all_outcome, alpha_all_outcome$State == state, 
                                 select = c(Hospital.Name, State))[t[[state]],]
            
            ## Append pairs of state names and the corresponding best hospitals to the data
            ## frame "all"
            all <- rbind(all, state_best)
        }
    }
    
    else if(num == "worst"){
        t <- tapply(alpha_all_outcome[[cap_outcome]], alpha_all_outcome[["State"]], 
                    which.max)
        for(state in names(t)){
            state_worst <- subset(alpha_all_outcome, alpha_all_outcome$State == state, 
                                 select = c(Hospital.Name, State))[t[[state]],]
            all <- rbind(all, state_worst)
        }
    }
    
    else{
        ## Important: apply as.numeric() to convert the character class to the numeric class
        t <- tapply(as.numeric(alpha_all_outcome[[cap_outcome]]), alpha_all_outcome[["State"]],
                    order)
        for(state in names(t)){
            ## The "t" is a list object that stores all states. Each state has its own list
            ## which stores the indices of hospitals sorted by ranking. That being said, the
            ## first element in each state is the best hospital, but the number is the index
            ## before sorting.
            state_rank_nth <- subset(alpha_all_outcome, alpha_all_outcome$State == state, 
                                     select = c(Hospital.Name, State))[t[[state]][num],]
            all <- rbind(all, state_rank_nth)
        }
    }
    
    names(all) <- c("hospital", "state")
    return(all) 
}