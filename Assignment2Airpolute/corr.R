corr <- function(directory, threshold = 0) {
    ############################ Function prototype from Coursera #############
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    ###########################################################################
    ## My code start here:
    ## Note: a simpler method is to call the complete() function in this file
    ## using source(), but in this work, I didn't do this just to have more
    ## coding work
    
    ## 1. Obtain file index
    monitorNames <- dir(directory)
    
    ## 2. initialize the result vector "res"
    res <- vector(mode = "numeric"); # mode has to be numeric!
    
    ## 3. access each monitor
    for (fIndex in 1:length(monitorNames)) {
        ## read the two critial columns
        fData <- read.csv( paste( c( directory, monitorNames[fIndex] ), collapse = .Platform$file.sep 
        ) )[ c( "sulfate", "nitrate" ) ]
        
        ## count how many rows are both non-NA
        nobs <- sum( !is.na(fData[,1]) & !is.na(fData[,2]) )
        
        ## judge whether the nobs is over the threshold
        if(nobs > threshold){
            ## calculate the correlation
            res <- c( res, cor( fData[,1], fData[,2], use = "complete.obs" ) )
        }
    }
    res
}