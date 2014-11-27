best <- function(state, outcome) {
    ## Read outcome data

    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
        
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

    # Get the hospitals with min patients and select the first in 
    # alphabetical order
    z <- w[w == min(w)]
    z <- sort(names(z))
    print(z[1])
}
