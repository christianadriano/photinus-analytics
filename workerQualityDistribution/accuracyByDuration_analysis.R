
#Is duration correlated with accuracy of answers?
library(dplyr);

countCorrectAnswerByDuration<-function(data){
  subsetTPTN_df = subset(data,select= c(Answer.duration_seconds, TP, TN));
  CorrectAnswers = subsetTPTN_df$TP + subsetTPTN_df$TN;
  subsetTPTN_df = data.frame(subsetTPTN_df);
  subsetTPTN_df["CorrectAnswers"]<-CorrectAnswers;
  
  by_questions = group_by(subsetTPTN_df,Answer.duration_seconds) ;
  summaryTable = summarize(by_questions,sum(CorrectAnswers));
  colnames(summaryTable) = c("Answer.duration_seconds","CorrectAnswers");
  
  return(summaryTable);
}

#################################################################################
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf <- loadAnswers();

summaryTable <- countCorrectAnswerByDuration(data = dataf);

#Normality tests 
shapiro.test(summaryTable$Answer.duration_seconds); 
#W = 0.41263, p-value < 2.2e-16 NOT NORMAL
qqnorm(y=summaryTable$Answer.duration_seconds);
qqline(summaryTable$Answer.duration_seconds);
#right skewed

shapiro.test(summaryTable$CorrectAnswers); 
#W = 0.89149, p-value = 0.0006019 NOT NORMAL
qqnorm(y=summaryTable$CorrectAnswers);
qqline(summaryTable$CorrectAnswers);
#Slight heavy tail 

output<- cor.test(summaryTable$Answer.duration_seconds,summaryTable$CorrectAnswers,method="kendall",use="pairwise");
output$p.value
#0.1227876 not Significant

output$estimate
#-0.02493711 negative correlation

