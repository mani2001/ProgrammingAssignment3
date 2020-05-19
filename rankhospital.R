#rankhospital function for ranking hospitals
rankhospital<-function(state,outcome,num="best")
{
  #read outcome data
  data<-read.csv("outcome-of-care-measures.csv")
  #seperating states and stating different outcomes
  States <- levels(factor(data[, 7]))
  outcomes<-c("heart attack","heart failure","pneumonia")
  #error if state or outcome is invalid
  if ((state %in% States) == FALSE) {return(NA)}
  else if ((outcome %in% outcomes) == FALSE) {return(NA)}
  #returns NA if value of num is > the length of the data
  if (is.numeric(num) == TRUE) {
    if (length(data[,2]) < num) {return(NA)}
  }
  #assign column number as per outcome
  colnumber<-if((outcome=="heart attack")){11}
  else if(outcome=="heart failure"){17}
  else {23}
  data[, colnumber] <- suppressWarnings(as.numeric(levels(data[, colnumber])[data[, colnumber]]))
  data[, 2] <- as.character(data[, 2])
  #subset data as per state name given by user
  dataset = subset(data,State==state)
  #take mortality values in a variable
  datacolumns<-suppressWarnings(as.numeric(dataset[,colnumber]))
  #remove na values
  dataset<-dataset[!(is.na(datacolumns)),]
  rankedHospital <-dataset[order(dataset[, colnumber], dataset[, 2]), ]
  if(num == "best") {numRank = 1}
  else if(num == "worst") {numRank = nrow(rankedHospital)}
  else{numRank = num}
  return(rankedHospital[numRank,2])
}