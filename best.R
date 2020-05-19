#function best for providing the hospital with best mortality rate (i.e. lowest)
best<-function(name,condition)
{
  #read the table
  data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  #seperate it based on states
  States <- levels(factor(data[, 7]))
  outcomes<-c("heart attack","heart failure","pneumonia")
  #error if state or outcome is invalid
  if ((name %in% States) == FALSE) {stop(print("-- invalid state input! --"))}
  else if ((condition %in% outcomes) == FALSE) {stop(print("-- invalid outcome input! --"))}
  #assign column number as per outcome
  colnumber<-if((condition=="heart attack")){11}
  else if(condition=="heart failure"){17}
  else {23}
  #subset data as per state name given by user
  dataset = subset(data,State=name)
  #take mortality values in a variable
  datacolumns<-as.numeric(dataset[,colnumber])
  #remove na values
  dataset<-dataset[!(is.na(datacolumns)),]
  datacolumns = dataset[,colnumber]
  #row/rows which have minimum mortality rate
  datarows = which(datacolumns==min(datacolumns))
  hospital<-dataset[datarows,2]
  #return the sorted hosiptal name
  return(sort(hospital))
}
