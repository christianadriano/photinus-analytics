
#Is age correlated with accuracy of answers
library(dplyr);

countCorrectAnswerByYoE<-function(data){
  subsetTPTN_df = subset(data,select= c(Worker.yearsOfExperience, TP, TN));
  CorrectAnswers = subsetTPTN_df$TP + subsetTPTN_df$TN;
  subsetTPTN_df = data.frame(subsetTPTN_df);
  subsetTPTN_df["CorrectAnswers"]<-CorrectAnswers;
  
  by_questions = group_by(subsetTPTN_df,Worker.yearsOfExperience) ;
  summaryTable = summarize(by_questions,sum(CorrectAnswers));
  colnames(summaryTable) = c("Worker.yearsOfExperience","CorrectAnswers");
  
  return(summaryTable);
}

#################################################################################
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf <- loadAnswers();

summaryTable <- countCorrectAnswerByYoE(data = dataf);

#Normality tests 
shapiro.test(summaryTable$Worker.yearsOfExperience); 
#W = 0.93878, p-value = 0.05138 NORMAL
qqnorm(y=summaryTable$Worker.yearsOfExperience);
qqline(summaryTable$Worker.yearsOfExperience);
#Slight heavy tail 

shapiro.test(summaryTable$CorrectAnswers); 
#W = 0.89149, p-value = 0.0006019 NOT NORMAL
qqnorm(y=summaryTable$CorrectAnswers);
qqline(summaryTable$CorrectAnswers);
#Slight heavy tail 

output<- cor.test(summaryTable$Worker.yearsOfExperience,summaryTable$CorrectAnswers,method="kendall",use="pairwise");
output$p.value
#0.01185164 Significant

output$estimate
# -0.3000108 medium correlation

