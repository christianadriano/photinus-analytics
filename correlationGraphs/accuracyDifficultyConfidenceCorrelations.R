#
# Compute correlation among difficulty and confidence and accuracy
#

loadAnswers<- function(fileName){
  
  setwd("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//correlationGraphs//");
  data_all <- read.csv(fileName,header = TRUE,sep=",");
  dataf = data.frame(data_all);
  return(dataf);
}

dataf<- loadAnswers("answerList_photinus_data.csv");
#Check if total answers is 2580
length(dataf$TP)




#Select TP, TN, FP, FN for the item
selectOutcomes <- function(dataf,lineNumbers){
  dataf_confidence <- dataf [(lineNumbers) ,];
  TP <- sum(dataf_confidence$TP);
  TN <- sum(dataf_confidence$TN);
  FP <- sum(dataf_confidence$FP);
  FN <- sum(dataf_confidence$FN);
  outcome.df <- data.frame(TP,TN,FP,FN);
  return (outcome.df);
}

#Compute accuracy for each level of difficulty and confidence
computeAccuracy<- function(df){
  return((df$TP+df$TN)/(df$TP+df$TN+df$FP+df$FN));
}

#Compute precision for each level of difficulty and confidence
computePrecision<- function(df){
  return((df$TP)/(df$TP+df$FP));
}

#Compute precision for each level of difficulty and confidence
computeRecall<- function(df){
  return((df$TP)/(df$TP+df$FN));
}

computeStats<- function(df,selection, filterName){
  
  dataframe <- data.frame(one=double(),two=double(), three=double(), four=double(), five=double());
  
  for(i in 1:5){
    outcomesDF<- selectOutcomes(df,selection==i);
    dataframe[1,i]<-c(computeAccuracy(outcomesDF));
    dataframe[2,i]<-c(computePrecision(outcomesDF));
    dataframe[3,i]<-c(computeRecall(outcomesDF));
    dataframe[4,i]<-c(sum(outcomesDF[1,]));
  }
  print(filterName);
  print(dataframe);
}


## ALL answers
stats<- computeStats(dataf,dataf$Answer.confidence,"All Answers - confidence");
sum(stats[4,])

## Ignoring IDK answers
dataIDK <- dataf [!(dataf$Answer.option=="IDK") ,];
stats<- computeStats(dataIDK,dataIDK$Answer.confidence,"All Answers - confidence");

#Ignore the IDK answers because we cannot evaluate their accuracy
dataf <- dataf [!(dataf$Answer.option=="IDK") ,];

