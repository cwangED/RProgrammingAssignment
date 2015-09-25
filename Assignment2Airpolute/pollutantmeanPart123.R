pollutantmean <- function(directory, pollutant, id = 1:332) {
    ############## Function prototype from the website ##############################
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    ####################################################################################  
    "set number of digits"
    options(digits = 4)
    ## 0. check the input
    if(!identical("sulfate", pollutant) && !identical("nitrate", pollutant)){
        ## if the pollutant is not matching with both targets, returh an error
        stop(paste(c("Pollutant is not recognized! Only use", dQuote("nitrate"), "or", dQuote("sulfate")), collapse = ""))
    }
#############
##    if(max(id)>232 || min(id)<1) {
##        ## check whether there are illegal ids
##        stop("Illegal id exists! Use")
##    }

        ## 1 index all monitors
    monitorNames <- dir(directory)
    
    ## ( The data is not huge and my computer can definitely read the data.
    ## However, to allow it read in more data, I limit the size of the 
    ## data loaded in to my physical memory to one .csv file in the specdata here.)
    
    dataSum <- 0
    dataNum <- 0
    for(fIndex in id) {
        ## 2 read the specified field and add them together 
        fData <- read.csv( paste( c( directory, monitorNames[fIndex] ), 
                                  collapse = .Platform$file.sep ) )[pollutant]
        dataSum <- dataSum + sum(fData, na.rm = TRUE)
        dataNum <- dataNum + sum(!is.na(fData))
    }
    ## 3. Final result
    dataSum/dataNum  
    
    ## if we don't round the result as required, the output number of digits was set to  7
    ## by default, thus to exactly obtain the results shown in the website example, we need
    ## to use the command: options(digits=4)

}