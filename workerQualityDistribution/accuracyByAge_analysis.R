
#Is age correlated with accuracy of answers
library(dplyr);


#Count number of correct answers by age
countCorrectAnswerByAge<-function(data){
  subsetTPTN_df = subset(data,select= c(Worker.age, TP, TN));
  CorrectAnswers = subsetTPTN_df$TP + subsetTPTN_df$TN;
  subsetTPTN_df = data.frame(subsetTPTN_df);
  subsetTPTN_df["CorrectAnswers"]<-CorrectAnswers;
  
  by_questions = group_by(subsetTPTN_df,Worker.age) ;
  summaryTable = summarize(by_questions,sum(CorrectAnswers));
  colnames(summaryTable) = c("Worker.age","CorrectAnswers");
  
  return(summaryTable);
}


#################################################################################
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf <- loadAnswers();

summaryTable <- countCorrectAnswerByAge(data = dataf);

#Normality tests 
shapiro.test(summaryTable$Worker.age); 
#W = 0.9731, p-value = 0.3876 NORMAL
qqnorm(y=summaryTable$Worker.age);
qqline(summaryTable$Worker.age);
#Slight heavy tail 

shapiro.test(summaryTable$CorrectAnswers); 
#W = 0.89149, p-value = 0.0006019 NOT NORMAL
qqnorm(y=summaryTable$CorrectAnswers);
qqline(summaryTable$CorrectAnswers);
#Slight heavy tail 

output<- cor.test(summaryTable$Worker.age,summaryTable$CorrectAnswers,method="kendall",use="pairwise");
output$p.value
#1.742642e-09 Significant

output$estimate
#-0.6313041 Stronlgy correlated


#-0.6313041 Stronlgy correlated
