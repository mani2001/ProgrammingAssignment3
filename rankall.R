#rankall function
rankall<-function(outcome,num="best")
{
  data<-read.csv("outcome-of-care-measures.csv")
  #seperating states and stating different outcomes
  States <- levels(factor(data[, 7]))
  outcomes<-c("heart attack","heart failure","pneumonia")
  #error if state or outcome is invalid
   if ((outcome %in% outcomes) == FALSE) {return(NA)}
  #returns NA if value of num is > the length of the data
  if (is.numeric(num) == TRUE) {
    if (length(data[,2]) < num) {return(NA)}
  }
  df<-data.frame()
  for(i in States){df1<-data.frame(i,rankhospital(i,outcome,num))
                                   df<-rbind(df,df1)}
  return(df)
}