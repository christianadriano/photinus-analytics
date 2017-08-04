

library(dplyr);

#Auxiliary functions

##by_questions<- group_by(subset_df,Question.ID) ;
countAnswerOptions<- function(dataf){
  subset_df <-subset(dataf,select= c(Answer.option));
  subset_df <- data.frame(subset_df); 
  
  #mark all rows that match the selection
  YesCount<- rowSums(subset_df=="YES"); 
  subset_df["YesCount"] <- YesCount;
  NoCount<- rowSums(subset_df=="NO"); 
  subset_df["NoCount"] <- NoCount;
  IDKCount<- rowSums(subset_df=="IDK"); 
  subset_df["IDKCount"] <- IDKCount;
  
  subset_df["QuestionID"] <- dataf$Question.ID;
  
  subset_df <-subset(subset_df,select= c(QuestionID,YesCount,NoCount,IDKCount));
  
  ##Group by question ID, which means adding all the values from the parallel columns in order to have
  #a sum for each questionID
  
  question_by <- group_by(subset_df,QuestionID);
  summaryTable<- summarize(question_by,TotalYes = sum(YesCount),TotalNo = sum(NoCount), TotalIDK = sum(IDKCount));
  
  colnames(summaryTable)<-c("Question.ID","Yes.Count","No.Count","IDK.Count");
  
  return(summaryTable);
}


computeMajorityVote<- function(summaryTable){
  summaryTable["MajorityVote"] <- summaryTable$Yes.Count - summaryTable$No.Count;
  return(summaryTable);
}

computeThresholdVote<- function(summaryTable, threshold){
  summaryTable["ThresholdVote"] <- summaryTable[,"Yes.Count"]>threshold;
  return(summaryTable);
}

#Add a column with the information of whether the question.ID 
#covers a bug of not.
appendGroundTruth<- function(summaryTable, questionList){
  list <- summaryTable[,"Question.ID"];
  summaryTable[,"bugCovering"] <- list$Question.ID %in% questionList;
  return(summaryTable);  
}

setJavaMethodID<- function(summaryTable){
  
  summaryTable<- setRangeID(summaryTable,1,c(0:9));
  summaryTable<- setRangeID(summaryTable,2,c(10:15));
  summaryTable<- setRangeID(summaryTable,3,c(16:32));
  summaryTable<- setRangeID(summaryTable,4,c(33:69));
  summaryTable<- setRangeID(summaryTable,5,c(70:78));
  summaryTable<- setRangeID(summaryTable,6,c(79:96));
  summaryTable<- setRangeID(summaryTable,7,c(97:104));
  summaryTable<- setRangeID(summaryTable,8,c(105:128));
  
  return (summaryTable);
}

setRangeID<-function(summaryTable,id, range){
  
  for(i in 1:length(range)){
    matchedRows <- which(summaryTable$Question.ID == range[i]);  
    summaryTable[matchedRows,"JavaMethod"]<-id;
  }
  return(summaryTable);
}

#Provides the list of questions that have to be considered 
#and the list of questions that covers bugs
computeRanking <- function(summaryTable){
  
  for(i in 1:8){
    selection <- summaryTable[summaryTable$JavaMethod==i,];
    
    #Sort 
    selection <- selection[with(selection,order(-Yes.Count)),];
    #remove duplicates
    uniqueLevels <- unique(selection$Yes.Count)
    labels<-matrix(NA,length(uniqueLevels),2);
    
    labels[,1]<- uniqueLevels;
    labels[,2]<- c(1:length(uniqueLevels));
    
    selection <- rankQuestions(selection,labels);
    
    summaryTable[selection$Question.ID+1,"ranking"]<-selection$ranking; 
  }
  
  return(summaryTable);
}


rankQuestions <- function(selection, labels){
  
  for(i in 1:length(labels[,2])){
    matchedRows <- which(selection$Yes.Count==labels[i,1]);  
    selection[matchedRows,"ranking"]<-labels[i,2];
  }
  
  return(selection);
}

#Select only the rows that match the questionIDlist
selectRows <-function(summaryTable,questionIDList){
  selectedRows <- summaryTable[summaryTable$Question.ID %in% questionIDList,];
  return(selectedRows);
}

######## Main code

runMain<-function(){
  
  # Import data
  source("C://Users//chris//OneDrive//Documentos//GitHub//randomForestWorkerConfidenceDifficulty//loadAnswers.R");
  dataf <- loadAnswers("answerList_data.csv");
  
  # Initialize Java method questions and bug covering data
  questionList <- c(1,4,10,14,20,23,30,32,55,56,57,58,59,72,73,77,84,92,95,97,102,104,115,119,123);
  
  
  summaryTable <- countAnswerOptions(dataf);
  summaryTable <- appendGroundTruth(summaryTable,questionList);
  summaryTable<- setJavaMethodID(summaryTable);
  summaryTable <- computeMajorityVote(summaryTable);
  summaryTable <- computeThresholdVote(summaryTable,6);
  summaryTable<- computeRanking(summaryTable);
  
  return (summaryTable);
}

##Run random forest
install.packages('randomForest')
library(randomForest)
install.packages("rpart.plot")
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rpart)
library(RColorBrewer)
library(rattle)
library(rpart.plot) 


totalData = length(summaryTable$Question.ID);
trainingSize = trunc(totalData * 0.7);
startTestIndex = totalData - trainingSize;
endTestIndex = totalData;

model <- randomForest(as.factor(bugCovering) ~ ranking, 
                      data = as.data.frame(summaryTable[1:trainingSize,]), 
                      importance=TRUE, ntree=2000, type="class");
varImpPlot(model);

# Predict YES answers
Prediction <- predict(model, test,'vote');
submit <- data.frame(Question.ID = test$Question.ID, PredictedLevel = Prediction, Actual = test$bugCovering);
write.csv(submit, file = "C://Users//chris//OneDrive//Documentos//GitHub//randomForestWorkerConfidenceDifficulty//firstforest_ranking.csv", row.names = FALSE);
model$predicted
model$confusion
model$votes

#---------------------------------------------------------------------

library(party)

# Decision Tree
fit <- rpart(as.factor(bugCovering) ~ ranking, method="class",
             data = as.data.frame(summaryTable));
#              data = as.data.frame(summaryTable[1:trainingSize,]));


printcp(fit);
plotcp(fit);
summary(fit);
plot(fit)
plot(fit, uniform=TRUE,main="Classification of bugCovering by ranking");
text(fit, use.n = TRUE, all=TRUE, cex=1);
#ranking higher than 2.5 implies non-bug. 
#Interpret these results from summary
#Run the decision tree for majority and threshold



