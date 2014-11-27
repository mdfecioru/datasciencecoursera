
pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)

    getVectorData <- function(index) {
        data_file_name <- paste(sprintf("%03d", index), '.csv', sep = '', collapse = '');
        data_file_name<-paste(getwd(), '/', directory, '/', data_file_name, sep = '', collapse = '');
        data <- read.csv(data_file_name);
        vect <- as.vector(data[pollutant]);
    }
    
    curateVector <- function(vect) {
        bad <- is.na(vect);
        vect <- vect[!bad];        
    }
    
    result <- c();
    for (i in id) {
        vect <- getVectorData(i);
        vect <- curateVector(vect);
        result <- c(result, vect);
    }
    
    print(sum(result) / length(result));
}

pollutantmean("specdata", "nitrate", 23);

