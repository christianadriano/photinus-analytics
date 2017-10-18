
#Is source code size correlated with accuracy of answers?
library(dplyr);
library(stringi);

countCorrectAnswerByCodeSize<-function(data){
  subsetTPTN_df = subset(data,select= c(Code.LOC, TP, TN));
  CorrectAnswers = subsetTPTN_df$TP + subsetTPTN_df$TN;
  subsetTPTN_df = data.frame(subsetTPTN_df);
  subsetTPTN_df["CorrectAnswers"]<-CorrectAnswers;
  
  by_questions = group_by(subsetTPTN_df,Code.LOC) ;
  summaryTable = summarize(by_questions,sum(CorrectAnswers));
  colnames(summaryTable) = c("Code.LOC","CorrectAnswers");
  
  return(summaryTable);
}

#################################################################################
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf <- loadAnswers();

summaryTable <- countCorrectAnswerByCodeSize(data = dataf);

#Normality tests 
shapiro.test(summaryTable$Code.LOC); 
#W = 0.9268, p-value = 0.3474 NORMAL
qqnorm(y=summaryTable$Code.LOC);
qqline(summaryTable$Code.LOC);
#normal

shapiro.test(summaryTable$CorrectAnswers); 
#W = 0.89149, p-value = 0.0006019 NOT NORMAL
qqnorm(y=summaryTable$CorrectAnswers);
qqline(summaryTable$CorrectAnswers);
#Slight heavy tail 

output<- cor.test(summaryTable$Code.LOC,summaryTable$CorrectAnswers,method="kendall",use="pairwise");
output$p.value
#  0.01614143 Significant

output$estimate
#-0.5343667  correlation

