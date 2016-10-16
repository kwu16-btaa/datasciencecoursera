rankall <- function(outcome, num = "best") {
    ## Read outcome data
    o_data <- read.csv("ProgAssignment3/outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that data is available
    if (all(is.na(o_data))) {
        stop("outcome data frame is empty. Most likely data was not read correctly.")
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
    all_data <- subset(o_data, o_data[colnum] != "Not Available", c(2,7,colnum))
    
    ## give data short colum names
    names(all_data) <- c("Hospital", "State", "Rate")
    ## sort the data according to the name of the hospitals
    all_data <- all_data[order(all_data$Hospital),]
    ## add a ranking column, need to convert output of ave from factor to an
    ## integer vector. when Ranking column is of factor type,
    ## it creates all kinds of issues when applying
    ## function such as min or max to it.
    all_data <- with (all_data, transform(all_data,
                                   Ranking = as.integer(ave(Rate, State, 
                            FUN = function(x) 
                                rank(as.numeric(x), ties.method = "first")))))
    split_data <-split(all_data, all_data$State)
    ## Return a data frama with two columns(Hospital, State) for the given rank
    if (num == "best") {
        split_data <- lapply(split_data,function(x) 
                    {x[x$Ranking == min(x$Ranking),]})
    }
    else if (num == "worst") {
         split_data <- lapply(split_data,function(x) {   
                                                    x[which.max(x$Ranking),]})
        #split_data <- lapply(split_data,function(x) {  
       #        subset(x, Ranking == max(Ranking))})
    }
    else {
        split_data <- lapply(split_data,function(x) 
        {subset(x, Ranking == num)})
    }
    
    df <- do.call("rbind", split_data)
    df[,c(1,2,4)]
}