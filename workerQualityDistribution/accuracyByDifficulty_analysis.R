
#Is difficulty correlated with accuracy of answers?
library(dplyr);

countCorrectAnswerByDifficulty<-function(data){
  subsetTPTN_df = subset(data,select= c(Answer.difficulty, TP, TN));
  CorrectAnswers = subsetTPTN_df$TP + subsetTPTN_df$TN;
  subsetTPTN_df = data.frame(subsetTPTN_df);
  subsetTPTN_df["CorrectAnswers"]<-CorrectAnswers;
  
  by_questions = group_by(subsetTPTN_df,Answer.difficulty) ;
  summaryTable = summarize(by_questions,sum(CorrectAnswers));
  colnames(summaryTable) = c("Answer.difficulty","CorrectAnswers");
  
  return(summaryTable);
}

#################################################################################
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf <- loadAnswers();

summaryTable <- countCorrectAnswerByDifficulty(data = dataf);

#Normality tests 
shapiro.test(summaryTable$Answer.difficulty); 
#W = 0.98189, p-value = 0.9672  NORMAL
qqnorm(y=summaryTable$Answer.difficulty);
qqline(summaryTable$Answer.difficulty);
#normal 

shapiro.test(summaryTable$CorrectAnswers); 
#W = 0.89149, p-value = 0.0006019 NOT NORMAL
qqnorm(y=summaryTable$CorrectAnswers);
qqline(summaryTable$CorrectAnswers);
#Slight heavy tail 

output<- cor.test(summaryTable$Answer.difficulty,summaryTable$CorrectAnswers,method="kendall",use="pairwise");
output$p.value
#0.4833333 not Significant

output$estimate
#-0.4 negative correlation

