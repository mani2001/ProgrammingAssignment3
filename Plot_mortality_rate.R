#read the csv data
outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
#coerce the column as numeric
outcome[,11]<-as.numeric(outcome[,11])
#plot the histogram
hist(outcome[,11])
