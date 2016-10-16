rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    o_data <- read.csv("ProgAssignment3/outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that data is available
    if (all(is.na(o_data))) {
        stop("outcome data frame is empty. Most likely data was not read correctly.")
    }
    
    ## Check that state and outcome are valid
    states <- unique(o_data$State)
    if (!(toupper(state) %in% states)) {
        stop("Please provide a valid state such as NJ or NY.")
    }
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    outcome <- tolower(outcome)
    if (!outcome %in% outcomes) {
        stop("Invalid outcome.")
    }
    
    ## Filter the outcome data by the state and only keep below columns: 
    ## col 2 (Hosptial name), col 7 (state name)
    ## col 11 (30-day mortality rate - heart attack)
    ## col 17 (30-day mortality rate - heart failure)
    ## col 23 (30-day mortality rate - pneumonia)
    if (outcome == "heart attack") colnum <- 11
    else if (outcome == "heart failure") colnum <- 17
    else if (outcome == "pneumonia") colnum <- 23
    state_data <- subset(o_data, o_data$State == toupper(state) & 
                             o_data[colnum] != "Not Available", c(2,colnum))
    ## give data short colum names
    names(state_data) <- c("Hospital", "Rate")
    ## sort the data according to the name of the hospitals
    state_data <- state_data[order(state_data$Hospital),]
    ## add a ranking column
    state_data <- transform(state_data,
              Ranking = rank(as.numeric(state_data$Rate),ties.method = "first"))
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (num == "best") {
        with(state_data, Hospital[which.min(Ranking)])
    }
    else if (num == "worst") {
        with(state_data, Hospital[which.max(Ranking)])
    }
    else {
        with(state_data, Hospital[Ranking == num])
    }
    
}