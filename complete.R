complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an interger vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame return a data frame 
    ## where the first column is the name of the file 
    ## and the second column is the number of complete cases.
    ## 
    
    complete_cases = numeric()
    
    ## Looping through the files required
    ## define a vector to store # of files read
    nofs <- 0
    for (i in id) {
        filename <- paste(directory, "/", sep="")
        if (i < 10) {
            filename <- paste(filename, "00", as.character(i), ".csv", sep ="")
        }
        else if (i >= 10 && i < 100) {
            filename <- paste(filename, "0", as.character(i), ".csv", sep ="")
        }
        else {
            filename <- paste(filename, as.character(i), ".csv", sep ="")
        }
        ##print(filename)
        file_data <- read.csv(filename)
        ok <- complete.cases(file_data[["sulfate"]], file_data[["nitrate"]])
        nofs <- nofs + 1
        complete_cases[nofs] = sum(ok)
    }
    ## print(length(complete_cases))
    ## Return the data frame with ids and completed cases
    data.frame(id = id, nobs = complete_cases)
}