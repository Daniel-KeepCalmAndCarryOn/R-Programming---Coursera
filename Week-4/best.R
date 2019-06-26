best <- function(state, outcome){
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Use stop() instead of message() to output error message
    if(!(state %in% data$State)){
        stop("invalid state")
    }
    if(!(tolower(outcome) %in% c("heart attack", "heart failure", "pneumonia"))){
        stop("invalid outcome")
    }
    
    ## Capitalize each word in the outcome string for further matching with the column name 
    ## that will be modified in the following code
    cap_outcome <- tools::toTitleCase(outcome)
    
    ## Subset useful data
    state_outcome <- subset(data, data$State == state, select = 
                                c(Hospital.Name, State, 
                                  Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                  Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                  Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    
    ## Modified the column names to avoiding writing multiple if-else statements for various 
    ## outcomes
    names(state_outcome) <- c("Hospital.Name", "State", "Heart Attack", "Heart Failure", 
                              "Pneumonia")
    
    ## Sort by alphabetical order
    alpha_state_outcome <- state_outcome[order(state_outcome$Hospital.Name),]
    
    ## The class of each column is character when data is read. To sort by numeric values, 
    ## so the class of the outcome column needs to be converted into numeric using as.numeric.
    ## The function which.min() returns the index of the minimum value.
    ## The function suppressWarnings() prevents warning messages of "NAs introduced by 
    ## coercion" from popping out.
    best_idx <- which.min(suppressWarnings(as.numeric(alpha_state_outcome[[cap_outcome]])))
    
    ## Use $Hospital.Nmae[best_idx] instead of [best_idx, 1] for readbility
    return(alpha_state_outcome$Hospital.Name[best_idx])
}