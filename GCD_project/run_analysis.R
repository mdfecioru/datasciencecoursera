# Author: Mihai Fecioru [20 Dec 2015]

# How to use it to demonstrate the project requirements:
#
#       d<-merge_data()
#       dd<-extract_mean_std(d)
#       r<-get_mean_tidy_data(dd)
#       save_ds_to_file(r)
#
# - d is the data frame containing both the test and train data
# - dd is the data frame that contains only the mean and standard deviation for 
#   each measurement.
# - r is the result for Step 5 requirement: a tidy data set with the average of 
#   each variable for each activity and each subject.
# The last command saves r data frame in a test file on the disk.


# Reads the activity labels from a file into a data frame.
# The function receives as parameter the data type (test or train).
read_activity_labels <- function(type) {
    act_type <- read.table(paste("UCI HAR Dataset/", type, "/y_", type, ".txt", sep=""))
    act_type <- cbind(seq(1, nrow(act_type)), act_type)    
    names(act_type) <- c("sample_id", "activity_id")
    
    act_lbl <- read.table("UCI HAR Dataset/activity_labels.txt")
    names(act_lbl) <- c("activity_id", "activity_name")
    
    m <- merge(act_type, act_lbl)
    m<- m[,!(names(m) %in% c("activity_id"))]
    m <- m[order(m$sample_id),]
    m
}

# Reads the subject ids from a file into a data frame.
# The function receives as parameter the data type (test or train).
read_subjects <- function(type) {
    sbj <- read.table(paste("UCI HAR Dataset/", type, "/subject_", type, ".txt", sep=""))
    sbj <- cbind(seq(1, nrow(sbj)), sbj)    
    names(sbj) <- c("sample_id", "subject_id")
    sbj
}

# Reads the actual data (the 561-feature vector) from a file into a data frame.
# The function receives as parameter the data type (test or train).
read_data_set <- function(type) {
    d <- read.table(paste("UCI HAR Dataset/", type, "/X_", type, ".txt", sep=""))
    d <- cbind(seq(1, nrow(d)), d)        
    data_lbl <- read.table("UCI HAR Dataset/features.txt")
    lbls <- c("sample_id", as.vector(data_lbl[, 2]))
    names(d) <- lbls
    d
}

# Creates a data frame by merging all the data from files.
# The function receives as parameter the data type (test or train).
get_data <- function(type) {
    act <- read_activity_labels(type)
    sbj <- read_subjects(type)
    ds <- read_data_set(type)

    result <- merge(sbj, act)
    result <- merge (result, ds)
    result <- result[,!(names(result) %in% c("sample_id"))]
    result
}

# Gets the test and train data and generates a data set that contains unified data
merge_data <- function() {
    test_data <- get_data("test")
    train_data <- get_data("train")
    
    merged_data <- rbind(test_data, train_data)
    merged_data <- merged_data[order(merged_data$subject_id),]
    merged_data
}

# Selects only the feature labels that contain the "-mean()" and the "-std()" strings
select_mean_std_labels <- function() {
    lbl <- read.table("UCI HAR Dataset/features.txt")
    act_lbl <- as.vector(lbl[, 2])
    result <- c()
    for (i in seq(along = act_lbl)) {
        if (grepl("-mean\\(\\)", act_lbl[i]) ||
                grepl("-std\\(\\)", act_lbl[i])) {
            result <- c(result, act_lbl[i])
        }
    }
    result
}

# Extracts from the unified data set only the column containing mean and standard 
# deviation measurements
extract_mean_std <- function(ds) {            
    result <- ds[, c("subject_id", "activity_name", select_mean_std_labels())]
    result
}

# Implements Step 5: creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.
get_mean_tidy_data <- function(ds) {
    ll<-select_mean_std_labels()
    d <- melt(ds, id=c("subject_id", "activity_name"), measure.vars=ll)
    result<-dcast(d, subject_id + activity_name ~ variable,mean)
    result
}

# Saves a data set to a file on the disk.
save_ds_to_file <- function(ds) {
    write.table(ds, file = "result.txt", row.name=FALSE)
}
