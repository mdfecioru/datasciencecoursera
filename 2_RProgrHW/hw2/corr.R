corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations

    getCompleteCases <- function(index) {
        data_file_name <- paste(sprintf("%03d", index), '.csv', sep = '', collapse = '');
        data_file_name<-paste(getwd(), '/', directory, '/', data_file_name, sep = '', collapse = '');
        data <- read.csv(data_file_name);
        data <- data[complete.cases(data), ]
    }
    
    result <- c();
    for (i in 1:332) {
        data <- getCompleteCases(i);
        if (nrow(data) > threshold) {
            sulf <- as.vector(data$sulfate);
            nitr <- as.vector(data$nitrate);
            result <- c(result, cor(sulf, nitr));
        }            
    }
    result;
}

cr <- corr("specdata", 150);
head(cr);
