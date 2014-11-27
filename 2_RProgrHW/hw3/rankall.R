
getHospitalPerState <- function(data_set, state, num) {    
    x <- data_set[state]
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
        dd[, "Hospital"][1]
    }
    else if (num == "worst") {
        dd <- my_data[order(-my_data$Rate, my_data$Hospital), ]
        dd[, "Hospital"][1]
    } else {
        idx <- as.integer(num)
        if (is.na(idx)) stop("invalid index")
        dd <- my_data[order(my_data$Rate, my_data$Hospital), ]        
        dd[, "Hospital"][idx]
    }    
}


rankall <- function(outcome, num = "best") {
    ## Read outcome data

    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    outcome_to_column <- c(11, 17, 23)
    names(outcome_to_column) <- c("heart attack", "heart failure", "pneumonia")
    out_idx <- outcome_to_column[outcome]    
    if (is.na(out_idx)) stop("invalid outcome")
    
    d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Convert string to numeric
    d[, out_idx] <- as.numeric(d[, out_idx])
    
    # Select [patients, Hospitals] from the selected state
    x <- split(d[, c(out_idx, 2)], d[, 7])
    states <- unique(as.vector(d[, 7]))
    
    n <- length(states)
    h <- character(n)
    s <- character(n)
    for (i in 1:n) {
        hosp <- getHospitalPerState(x, states[i], num)
        h[i] <- toString(hosp)
        s[i] <- toString(states[i])
    }
   
    result <- data.frame(h, s, stringsAsFactors=FALSE)
    
    names(result)[1] = "hospital"
    names(result)[2] = "state"
    result[order(result$state), ]
}
