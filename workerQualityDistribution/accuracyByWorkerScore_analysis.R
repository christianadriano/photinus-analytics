
#Is Worker.score correlated with accuracy of answers?
library(dplyr);

countCorrectAnswerByScore<-function(data){
  subsetTPTN_df = subset(data,select= c(Worker.score, TP, TN));
  CorrectAnswers = subsetTPTN_df$TP + subsetTPTN_df$TN;
  subsetTPTN_df = data.frame(subsetTPTN_df);
  subsetTPTN_df["CorrectAnswers"]<-CorrectAnswers;
  
  by_questions = group_by(subsetTPTN_df,Worker.score) ;
  summaryTable = summarize(by_questions,sum(CorrectAnswers));
  colnames(summaryTable) = c("Worker.score","CorrectAnswers");
  
  return(summaryTable);
}

#################################################################################
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf <- loadAnswers();

summaryTable <- countCorrectAnswerByScore(data = dataf);

#Normality tests 
shapiro.test(summaryTable$Worker.score); 
#W = 1, p-value = 1 NOT NORMAL
qqnorm(y=summaryTable$Worker.score);
qqline(summaryTable$Worker.score);
#normal 

shapiro.test(summaryTable$CorrectAnswers); 
#W = 0.89149, p-value = 0.0006019 NOT NORMAL
qqnorm(y=summaryTable$CorrectAnswers);
qqline(summaryTable$CorrectAnswers);
#Slight heavy tail 

output<- cor.test(summaryTable$Worker.score,summaryTable$CorrectAnswers,method="kendall",use="pairwise");
output$p.value
#0.3333333 not Significant

output$estimate
#1  

