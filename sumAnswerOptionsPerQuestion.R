

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

computeThreshold<- function(summaryTable, threshold){
  summaryTable["ThresholdVote"] <- summaryTable[,"Yes.count"]>threshold;
  return(summaryTable);
}

#Add a column with the information of whether the question.ID 
#covers a bug of not.
appendGroundTruth<- function(summaryTable, questionList){
  list <- summaryTable[,"Question.ID"];
  summaryTable[,"bugCovering"] <- list$Question.ID %in% questionList;
  return(summaryTable);  
}



#Provides the list of questions that have to be considered 
#and the list of questions that covers bugs
computeRanking<- function(summaryTable,  questionRangeList){
  summaryTable["ThresholdVote"] <- summaryTable[,"Yes.count"]>threshold;
  return(summaryTable);
}


######## Main code

# Import data
source("C://Users//chris//OneDrive//Documentos//GitHub//randomForestWorkerConfidenceDifficulty//loadAnswers.R");
dataf <- loadAnswers("answerList_data.csv");

# Initialize Java method questions and bug covering data
questionList <- c(1,4,10,14,20,23,30,32,55,56,57,58,59,72,73,77,84,92,95,97,102,104,115,119,123);
JavaMethod1_questions <- c(1:9);
JavaMethod2_questions <- c(10:15); 
JavaMethod3_questions <- c(16:32);
JavaMethod4_questions <- c(33:69);
JavaMethod5_questions <- c(70:78);
JavaMethod6_questions <- c(79:96);
JavaMethod7_questions <- c(97:104);
JavaMethod8_questions <- c(105:128);

summaryTable <- countAnswerOptions(dataf);
summaryTable <- appendGroundTruth(summaryTable,questionList);


