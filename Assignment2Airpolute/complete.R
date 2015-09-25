complete <- function(directory, id = 1:332) {
    ############################## Function prototype from Coursera #####################
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    ######################################################################################
    ## My code start here:
    
    ## The question here is the definition of "complete"
    ## In this work, we assume that "complete" means none of "sulfate" and "nitrate" is NA
    
    ## 1. Obtain file index
    monitorNames <- dir(directory)
    
    ## 2. initialize the number of complete observe and a counter
    nobs <- vector(length = length(id))
    mCount <- 0;
    
    ## 3. access each monitor
    for (fIndex in id) {
        mCount = mCount+1
        ## read the two critial columns
        fData <- read.csv( paste( c( directory, monitorNames[fIndex] ), collapse = .Platform$file.sep 
                                  ) )[ c( "sulfate", "nitrate" ) ]
        ## count how many rows are both non-NA
        nobs[mCount] <- sum( !is.na(fData[,1]) & !is.na(fData[,2]) )
    }
    
    ## 4. combine the "id" and "nobs" vectors to form a matrix "res" means result
    res <- cbind(id, nobs)
    names(res)<- c("id", "nobs")
    res <- as.data.frame(res)
    res
    
}
