## This function reads in the outcome data
## and returns hosptial name in a state with lowest 30-day death rate

best <- function(state, outcome) {
    ## Read outcome data
    o_data <- read.csv("ProgAssignment3/outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (all(is.na(o_data))) {
        stop("outcome data frame is empty. Most likely data was not read correctly.")
    }
    states <- unique(o_data$State)
    if (!(toupper(state) %in% states)) {
        stop("Please provide a valid state abbreviation such as NJ or NY.")
    }
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    outcome <- tolower(outcome)
    if (!outcome %in% outcomes) {
        stop("Please pass a correct outcome.")
    }
    
    ## Filter the outcome data by the state and only keep 
    ## col 2 (Hosptial name), col 7 (state name) and 
    ## col 11 (30-day mortality rate - heart attack)
    ## col 17 (30-day mortality rate - heart failure)
    ## col 23 (30-day mortality rate - pneumonia)
    if (outcome == "heart attack") colnum <- 11
    else if (outcome == "heart failure") colnum <- 17
    else if (outcome == "pneumonia") colnum <- 23
    state_data <- subset(o_data, o_data$State == toupper(state) & 
                             o_data[colnum] != "Not Available", c(2,colnum))
    
    ## change the column name so we can refer it easily
    colnames(state_data) <- c("H","O")
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate for the given outcome
    best_h <- sort(state_data$H[which.min(state_data$O)])
    best_h[1]
}