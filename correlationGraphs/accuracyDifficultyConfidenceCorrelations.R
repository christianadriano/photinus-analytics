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
selectOutcomes <- function(df,lineNumbers){
  dataf_confidence <- df [(lineNumbers) ,];
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
sum(stats[4,]);

#### ANSWER OPTION
## Only YES's
dataYES <- dataf [(dataf$Answer.option=="YES") ,];
stats<- computeStats(dataYES,dataYES$Answer.confidence,"Only YES Answers - confidence");

## Only NO's
dataNO <- dataf [(dataf$Answer.option=="NO") ,];
stats<- computeStats(dataNO,dataNO$Answer.confidence,"Only NO Answers - confidence");

#### WORKER PROFESSION
## Only PROFESSIONAL_DEVELOPERS
dataProf <- dataf [(dataf$Worker.profession=="Professional_Developer") ,];
stats<- computeStats(dataProf,dataProf$Answer.confidence,"Only PROFESSIONAL_DEVELOPERS - confidence");
sum(stats[4,]);

## Only HOBBYIST´S
dataProf <- dataf [(dataf$Worker.profession=="Hobbyist") ,];
stats<- computeStats(dataProf,dataProf$Answer.confidence,"Only HOBBYIST - confidence");
sum(stats[4,]);

## Only GRADUATE_STUDENT´S
dataProf <- dataf [(dataf$Worker.profession=="Graduate_Student") ,];
stats<- computeStats(dataProf,dataProf$Answer.confidence,"Only GRADUATE_STUDENT - confidence");
sum(stats[4,]);

## Only UNDERGRADUATE_STUDENT´S
dataProf <- dataf [(dataf$Worker.profession=="Undergraduate_Student") ,];
stats<- computeStats(dataProf,dataProf$Answer.confidence,"Only UNDERGRADUATE_STUDENT - confidence");
sum(stats[4,]);

## Only OTHER´S
dataProf <- dataf [(dataf$Worker.profession=="Other") ,];
stats<- computeStats(dataProf,dataProf$Answer.confidence,"Only OTHER - confidence");
sum(stats[4,]);


#### JAVA METHOD


