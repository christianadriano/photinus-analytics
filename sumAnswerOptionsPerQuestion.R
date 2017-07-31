library(dplyr);

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
  
  colnames(summaryTable)<-c("Questoin.ID","Yes.Count","No.Count","IDK.Count");
  
  return(summaryTable);
}


computeMajorityVote(summaryTable){
  summaryTable["MajorityVote"] <- summaryTable$Yes.Count - summaryTable$No.Count;
  return(summaryTable);
}
  
computeThreshold(summaryTable, threshold){
  summaryTable["ThresholdVote"] <- summaryTable[,"Yes.count"]>threshold;
  return(summaryTable);
}
  
computeRanking(summaryTable, threshold){
  summaryTable["ThresholdVote"] <- summaryTable[,"Yes.count"]>threshold;
  return(summaryTable);
}
  