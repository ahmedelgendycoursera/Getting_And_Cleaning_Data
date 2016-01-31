#start
library(tools)
library(plyr)
Sys.setlocale("LC_ALL", "C")
loc.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#create a temporary folder.
td <- tempdir()
#create a temporary file.
tf <- tempfile(tmpdir=td, fileext=".zip")
#download the file using the link to the temporary file.
download.file(loc.url, tf)
#list the files and the folders in a zipped folder and store their names in a list.
filesNames <- unzip(tf, list=TRUE)$Name

#create an empty list to hold data frames from files with the word "test".
testList <- list()
#create a loop counter to be used in filling the list.
itest <- 1
#create an empty list to hold data frames from files with the word "train".
trainList <- list()
#create a loop counter to be used in filling the list.
itrain <- 1
#iterate through the list of unzipped files' names.
for(i in filesNames){
  #unzip the files in a zipped folder.
  unzip(tf, files=i, exdir=td, overwrite=TRUE)
  #check if the file name contains the extentions "txt" to make sure it is a file and not a folder.
  #because the "filesnames" contains files and folders.
  output1 <- grep("txt",i)
  #check if the file names contains the word "subject" because we do not want to split the columns out of these "subject" files.
  output2 <- grep("subject",i)
  # if the file name contains the word "test".
  output3 <- grep("test",i)
  # if the file name contains the word "train".
  output4 <- grep("train",i)
  if(length(output1)==1){
    #get the absolute path of the file.
    filePath <-  file.path(td,i)
    #parse the file name out from the file path and then parse the file name without the extension.
    fileNameNoExt <- file_path_sans_ext(basename(filePath))
    #read the file.
    data <- read.table(file = filePath,sep="\t",stringsAsFactors = FALSE)
    #if the file does not contain the word "subject", or not equal to "y_test" or "y_train", split the columns.
    if(length(output2)!=1|(i!="y_train"|i!="y_test")){
      #change the column to character because if you do not strsplit() will throw exception.
      data[,1] <- as.character(data[,1])
      #split the contents of the first column (only column) into several columns based on space character.
      data <- data.frame(do.call('rbind', strsplit((data[,1])," ",fixed=TRUE)))
      
    }
    
    #assign the data frame to the file name by converting the file name to a variable.
    assign(fileNameNoExt,data)
    
    # if the file contains the word "test", add the frame to the list.
    if(length(output3)==1){
      testList[[itest]] <- data
      #if the data frame is the first use it to initialize the big data frame
      if(itest==1){
        cbindTestData <- data
        # if it is not the first data frame, just cbind it to the big one and after the binding, it becomes the big one. 
      }else{
        cbindTestData <- cbind(cbindTestData,data)
      }
      itest <- itest + 1
    }
    
    # if the file contains the word "train", add the frame to the list.
    if(length(output4)==1){
      trainList[[itrain]] <- data
      
      if(itrain==1){
        cbindTrainData <- data
      }else{
        cbindTrainData <- cbind(cbindTrainData,data)
      }
      itrain <- itrain + 1
    }
    
    
    #print the file name that is done.
    print(fileNameNoExt)
  }else{
    #if the file does not contain "txt" then it must be a folder. Print "NO" to let us know it is a folder.
    print("No")
  }
}

#column-bind three data frames of same row numbers but different columns.
testData <- cbind(subject_test,y_test,X_test)
trainData <- cbind(subject_train,y_train,X_train)
#change the name of the columns to be used in cbind.fill as a bind index.
colnames(testData)[1] <- "subject"
colnames(testData)[2] <- "Y"
colnames(trainData)[1] <- "subject"
colnames(trainData)[2] <- "Y"
#row-bind the two frames of same first 802 columns but different row-numbers.
#rbind.fill from "plyr" library to force the binding despite the difference in the number of columns.
#first 802 columns are common between the two frames.But the result is the same as the bigger of the two which is 936.
master <- rbind.fill(testData,trainData)
#delete an empty column in the data frame.
master$X1 <- NULL
#create another copy of master data.
master2 <- master
#convert the column which contains column names in "feature" from factors to characters.
features$X2 <- as.character(features$X2)
#use the column (vector of characters) to name "master2" columns.
colnames(master2)[3:564] <- features$X2
#create an index vector of columns in master2 frame that contain the words "mean", "std", and or the columns that are equal to the word "subject" and the word "Y".
#we are trying here to select specific set of columns based on their names.
indexOfMstdColumns <- colnames(master2)[grepl(pattern= "mean",colnames(master2), ignore.case = TRUE)|grepl(pattern= "std",colnames(master2),  ignore.case = TRUE)|(colnames(master2)=="subject")|(colnames(master2)=="Y")]
#using the index vector, we extract from "master2" another data frame called "master3".
master3 <- master2[,(indexOfMstdColumns)]
#create a fourth copy of the data
master4 <- master3
#delete an unecessary column in "activity_labels" frame.
activity_labels$X1 <- NULL
#index the column of the primary key with the column of the foregin key in the other table.
#And then replace the column of the foregin key with the output.
master4$Y <- activity_labels$X2[master4$Y]
#change the 2nd column name to a descriptive name.
colnames(master4)[2] <- "activity"
master5 <- master4
#replace the special characters in master4 column names with one dot.
colnames(master5) <- gsub(pattern = "[(_);,-]+",replacement= ".", x = colnames(master4))
#create a new copy of data
master6 <- master5
#replace the dot at the end of the names with nothing
colnames(master6) <- gsub(pattern = "[.]$",replacement= "", x = colnames(master5))
#compute the mean of each variable per subject-actvity pair.
aggregate(. ~ subject + activity, data = master6, FUN=mean)
#end