#Assignment- Coursera R Programming Week2

#Q1
#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers,
#'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and 
#returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.


pollutantmean <- function(directory='specdata',pollutant='sulfate',id=1:332){
    
      data_join <- list.files(directory,pattern = "*.csv", full.names = TRUE) 
      master <- as.data.frame(do.call(rbind, lapply(data_join[id], read.csv, header = TRUE)))
      mean(master[,pollutant],na.rm=TRUE)
}

#Q2
#Write a function that reads a directory full of files and reports the number of completely observed cases in each data file.
#The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases

complete <- function(directory='specdata',id=1:332){
        
        data_join <- list.files(directory,pattern = "*.csv", full.names = TRUE) 
        master <- as.data.frame(do.call(rbind, lapply(data_join[id], read.csv, header = TRUE)))
        master <- master[!is.na(master[,'sulfate']),]
        master <- master[!is.na(master[,'nitrate']),]
        
        nobs<-rep(0,length(id)) 
        
        
for (i in 1:length(id)){

    nobs[i]<- nrow(master[master$ID==id[i],]) }
        
        complete.cases <- data.frame(id, nobs, row.names = NULL)
}

#Q3
#Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate 
#for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. 
#The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, 
#then the function should return a numeric vector of length 0
#This Function read multiple files from a directory
#and find a correlation between two columns of the data frame


corr <- function(directory,threshold=150){
      
        files_list <- list.files(directory, full.names=TRUE)
        mydata <- data.frame()
        treshhold<-vector(,1)
        cr<-vector(,332)
        
     
        for (i in 1:332){                                
                mydata <- read.csv(files_list[i])
                complete<-sum(complete.cases(mydata))
                
                if (complete > treshhold) {        
                        x<- cor(mydata$sulfate,mydata$nitrate,use="complete.obs")            
                        
                        cr<-c(x)
                        print(cr)
                } else{
                        x<-0
                }
        } 
}