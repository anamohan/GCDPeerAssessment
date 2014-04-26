RunAnalysis <- function() {
    testData   <- ReadDataFromDirectory("test")
    trainData  <- ReadDataFromDirectory("train")
    mergedData <- rbind(testData, trainData)

    ## Read features from features.txt
    features <- read.table("UCI HAR Dataset/features.txt")

    ## Discard features that are not mean or standard deviation
    ## Extract those features from merged Data
    columns <- apply(features,1, function(x) grepl("-mean\\(\\)|-std\\(\\)", x[2]))
    columns
    filteredData <- mergedData[,columns]

    ## Get the previous names from filtered data
    oldNames <- names(filteredData)
    feature.ids <- as.numeric(sapply(oldNames, function(x) gsub("V", "", x)))
    names <- sapply(feature.ids, function(id) features[id, 2])
    names(filteredData) <- sapply(names, function(x) tolower(gsub("[\\(\\)-]","", x)))

    ## Read the activity Data
    y.test <- read.table("UCI HAR Dataset/test/y_test.txt")
    y.train <- read.table("UCI HAR Dataset/train/y_train.txt")
    mergedYData <- rbind(y.test,y.train)
    activity <- apply(mergedYData,1, function(x) c("walking", "walking upstairs", "walking downstairs", "sitting", "standing","laying")[x])
    with.activity <- cbind(data.frame(activity), filteredData)

    ## Read the subject Data
    subject.test <- read.table("UCI HAR Dataset/test/subject_test.txt")
    subject.train <- read.table("UCI HAR Dataset/train/subject_train.txt")
    mergedSubjectData <- rbind(subject.test, subject.train)
    with.subject <- cbind(data.frame(subject = mergedSubjectData$V1), with.activity)

    require(reshape2)
    meltData <- melt(with.subject,id.vars=c("activity","subject"))     
    averaged.data <- dcast(meltData, subject + activity ~ variable,mean)
    write.table(averaged.data, file = "tidy_data_final.txt")
}

ReadDataFromDirectory <- function(set) {
    
    file <- paste0("UCI HAR Dataset/",set,"/X_",set,".txt")
    
    ## Read table if file exists
    if (file.exists(file)) {
        read.table(file)
    } else {
        ## return NULL if we are not able to find the file
        warning("File not found")
        return(NULL)
    }
}

RunAnalysis() 
