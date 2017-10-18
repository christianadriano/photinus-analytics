
#Is explanation size correlated with accuracy of answers?
library(dplyr);
library(stringi);

countCorrectAnswerByProfession<-function(data){
  subsetTPTN_df = subset(data,select= c(Worker.profession, TP, TN));
  CorrectAnswers = subsetTPTN_df$TP + subsetTPTN_df$TN;
  subsetTPTN_df = data.frame(subsetTPTN_df);
  subsetTPTN_df["CorrectAnswers"]<-CorrectAnswers;
  
  by_questions = group_by(subsetTPTN_df,Worker.profession) ;
  summaryTable = summarize(by_questions,sum(CorrectAnswers));
  colnames(summaryTable) = c("Worker.profession","CorrectAnswers");
  
  return(summaryTable);
}

#################################################################################
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf <- loadAnswers();

dataf$Worker.profession <- as.numeric(dataf$Worker.profession)
summaryTable <- countCorrectAnswerByProfession(data = dataf);

#Normality tests 
shapiro.test(summaryTable$Worker.profession); 
#W = 0.98676, p-value = 0.9672 NORMAL
qqnorm(y=summaryTable$Worker.profession);
qqline(summaryTable$Worker.profession);
#right skewed

shapiro.test(summaryTable$CorrectAnswers); 
#W = 0.89149, p-value = 0.0006019 NOT NORMAL
qqnorm(y=summaryTable$CorrectAnswers);
qqline(summaryTable$CorrectAnswers);
#Slight heavy tail 

output<- cor.test(summaryTable$Worker.profession,summaryTable$CorrectAnswers,method="kendall",use="pairwise");
output$p.value
# 0.8166667 NOT Significant

output$estimate
#0.2 correlation

