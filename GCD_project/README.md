### Usage instructions

       d<-merge_data()
       dd<-extract_mean_std(d)
       r<-get_mean_tidy_data(dd)
       save_ds_to_file(r)

 * d is the data frame containing both the test and train data
 * dd is the data frame that contains only the mean and standard deviation for 
   each measurement.
 * r is the result for Step 5 requirement: a tidy data set with the average of 
   each variable for each activity and each subject.
 
 The last command saves r data frame in a test file on the disk.

### Assumptions / Comments

 * Data must be available in the same folder as the run_analysis.R script. Just
   unzip the data archive and put the "UCI HAR Dataset" folder in the same folder
   with the run_analysis.R script
 * For Step 2 (Extracts only the measurements on the mean and standard deviation 
   for each measurement), I've only selected the columns containing the "-mean()"
   and "-std()" strings.
