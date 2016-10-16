corr <- function(directory, thredshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all variables)
    ## required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of corrections 
  
    
    directory <- paste(getwd(), "/", directory, sep = "")
    
    ## Read the directory and stored all the files in a character vector
    filenames <- dir(directory)
    correlation = numeric()
    nfile = 0
    for (file in filenames) {
        file <- paste(directory, "/", file, sep="")
        file_data <- read.csv(file)
        ok <- complete.cases(file_data[["sulfate"]], file_data[["nitrate"]])
        if (sum(ok) > thredshold ) {
            nfile = nfile + 1
            correlation[nfile] <- cor(file_data[["sulfate"]][ok], file_data[["nitrate"]][ok])
        }
   }
    ## print(length(correlation))
    ## Return the correlation vector
    correlation
}