rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data

    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    outcome_to_column <- c(11, 17, 23)
    names(outcome_to_column) <- c("heart attack", "heart failure", "pneumonia")
    out_idx <- outcome_to_column[outcome]    
    if (is.na(out_idx)) stop("invalid outcome")
    
    d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if (!is.element(state, d[, 7])) stop("invalid state")
    
    # Convert string to numeric
    d[, out_idx] <- as.numeric(d[, out_idx])
    
    # Select [patients, Hospitals] from the selected state
    x <- split(d[, c(out_idx, 2)], d[, 7])
    x <- x[state]
    x <- x[[1]]
    
    # Get patients per hospita and turn into a vector
    w <- sapply(split(x[, 1], x[, 2]), mean)
    
    # Eliminate NA values
    bad <- is.na(w)
    w <- w[!bad]    

    my_data <- data.frame(Hospital=names(w), Rate=as.vector(w))
    my_data[, "Hospital"] <- as.character(my_data[, "Hospital"])

    if (num == "best") {
        dd <- my_data[order(my_data$Rate, my_data$Hospital), ]
        print(dd[, "Hospital"][1])
    }
    else if (num == "worst") {
        dd <- my_data[order(-my_data$Rate, my_data$Hospital), ]
        print(dd[, "Hospital"][1])
    } else {
        idx <- as.integer(num)
        if (is.na(idx)) stop("invalid index")
        dd <- my_data[order(my_data$Rate, my_data$Hospital), ]        
        print(dd[, "Hospital"][idx])
    }
}
