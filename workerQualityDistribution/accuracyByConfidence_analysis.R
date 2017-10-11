
#Is confidence correlated with accuracy of answers?
library(dplyr);

countCorrectAnswerByConfidence<-function(data){
  subsetTPTN_df = subset(data,select= c(Answer.confidence, TP, TN));
  CorrectAnswers = subsetTPTN_df$TP + subsetTPTN_df$TN;
  subsetTPTN_df = data.frame(subsetTPTN_df);
  subsetTPTN_df["CorrectAnswers"]<-CorrectAnswers;
  
  by_questions = group_by(subsetTPTN_df,Answer.confidence) ;
  summaryTable = summarize(by_questions,sum(CorrectAnswers));
  colnames(summaryTable) = c("Answer.confidence","CorrectAnswers");
  
  return(summaryTable);
}

#################################################################################
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf <- loadAnswers();

summaryTable <- countCorrectAnswerByConfidence(data = dataf);

#Normality tests 
shapiro.test(summaryTable$Answer.confidence); 
#W = 0.98189, p-value = 0.9606 NORMAL
qqnorm(y=summaryTable$Answer.confidence);
qqline(summaryTable$Answer.confidence);
#Slight heavy tail 

shapiro.test(summaryTable$CorrectAnswers); 
#W = 0.89149, p-value = 0.0006019 NOT NORMAL
qqnorm(y=summaryTable$CorrectAnswers);
qqline(summaryTable$CorrectAnswers);
#Slight heavy tail 

output<- cor.test(summaryTable$Answer.confidence,summaryTable$CorrectAnswers,method="kendall",use="pairwise");
output$p.value
#0.002777778 Significant

output$estimate
# 1 strong correlation

