run_analysis <- function() {
    
    library(data.table)
    library(plyr)
    
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    # creates a temporary directory
    td = tempdir()
    # creates the placeholder file
    tf = tempfile(tmpdir=td, fileext=".zip")
    # downloads into the placeholder file
    download.file(fileUrl, tf, method = "curl")
    
    # gets the name of the files in the zip archive
    fname = unzip(tf, list=TRUE)$Name
    # unzips the files to the temporary directory
    unzip(tf, files=fname, exdir=td, overwrite=TRUE)
    # fpath is the full path to the extracted files
    fpath = file.path(td, fname)
    
    #reads the files
    features <- fread(fpath[2])
    train <- read.table(fpath[31])
    subject_train <- fread(fpath[30])
    activity_train <- fread(fpath[32])
    test <- read.table(fpath[17])
    subject_test <- fread(fpath[16])
    activity_test <- fread(fpath[18])
    activity_labels <- fread(fpath[1])
    
    #creates the data frame with the files
    data <- rbind(train,test)
    subject <- rbind(subject_train,subject_test)
    activity <- rbind(activity_train,activity_test)
    DT <- cbind(data,subject,activity)
    colnames(DT)[1:561]<-features$V2
    colnames(DT)[562:563]<-c("Subject", "Activity")
    
    #maps ID activities with a clean version of the activity names
    for (i in 1:6) {
        DT$Activity[DT$Activity == activity_labels$V1[i]] <- 
            sub("_", "", tolower(activity_labels$V2[i]))
    }
    
    #subset of DT for means and standard deviations
    mean_index <- grep("mean\\(", colnames(DT))
    std_index <- grep("std", colnames(DT))
    index <- c(mean_index,std_index,562,563)
    subDT <- DT[,index]
    
    #loop on measurements columns
    for (j in 1:66) {
        
        #cleaning of the column names
        name <- strsplit(colnames(subDT)[j], "-")
        if (name[[1]][2] == "mean()") {
            name[[1]][2] <- "Mean"
        } else if (name[[1]][2] == "std()") {
            name[[1]][2] <- "Std" 
        }        
        if (length(name[[1]]) == 3) {
            colnames(subDT)[j] <- paste(name[[1]][1],name[[1]][2],name[[1]][3], sep = "")
        } else {
            colnames(subDT)[j] <- paste(name[[1]][1],name[[1]][2], sep = "")   
        }
        
        
        #creation of the final tidy data frame
        if (j == 1) {
            FDF <- ddply(subDT, c("Subject","Activity"), function(d) data.frame(mean=mean(d[,colnames(d)[1]])))
        } else {
            df <- ddply(subDT, c("Subject","Activity"), function(d) data.frame(mean=mean(d[,colnames(d)[j]])))
            FDF[,j+2] <- df[,3]
        }
        
    }
    
    #Appending the colnames to the final dat frame
    colnames(FDF)[3:68] <- colnames(subDT)[1:66]
    
    #writing of tidy data in run_analysis.txt file
    write.table(FDF, "run_analysis.txt", row.names = F, quote = F)    
    
}