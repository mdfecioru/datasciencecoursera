complete <- function(directory, id = 1:332) {
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

    getCompleteCases <- function(index) {
        data_file_name <- paste(sprintf("%03d", index), '.csv', sep = '', collapse = '');
        data_file_name<-paste(getwd(), '/', directory, '/', data_file_name, sep = '', collapse = '');
        data <- read.csv(data_file_name);
        nrow(data[complete.cases(data), ]);
    }
    
    result <- data.frame(id= integer(0), nobs= integer(0));
    for (i in id)
        result <- rbind(result, c(i, getCompleteCases(i)));
    
    names(result)[1] = "id"
    names(result)[2] = "nobs"
    print(result);
}

complete("specdata", c(2, 4, 8, 10, 12));
