
#Is source code complexity correlated with accuracy of answers?
library(dplyr);
library(stringi);

countCorrectAnswerByCodeComplexity<-function(data){
  subsetTPTN_df = subset(data,select= c(Code.complexity, TP, TN));
  CorrectAnswers = subsetTPTN_df$TP + subsetTPTN_df$TN;
  subsetTPTN_df = data.frame(subsetTPTN_df);
  subsetTPTN_df["CorrectAnswers"]<-CorrectAnswers;
  
  by_questions = group_by(subsetTPTN_df,Code.complexity) ;
  summaryTable = summarize(by_questions,sum(CorrectAnswers));
  colnames(summaryTable) = c("Code.complexity","CorrectAnswers");
  
  return(summaryTable);
}

#################################################################################
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf <- loadAnswers();

summaryTable <- countCorrectAnswerByCodeComplexity(data = dataf);

#Normality tests 
shapiro.test(summaryTable$Code.complexity); 
#W = 0.88465, p-value = 0.2479 NORMAL
qqnorm(y=summaryTable$Code.complexity);
qqline(summaryTable$Code.complexity);
#normal

shapiro.test(summaryTable$CorrectAnswers); 
#W = 0.89149, p-value = 0.0006019 NOT NORMAL
qqnorm(y=summaryTable$CorrectAnswers);
qqline(summaryTable$CorrectAnswers);
#Slight heavy tail 

output<- cor.test(summaryTable$Code.complexity,summaryTable$CorrectAnswers,method="kendall",use="pairwise");
output$p.value
#0.002777778 Significant

output$estimate
#-0.9047619  correlation

