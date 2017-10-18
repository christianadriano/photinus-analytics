
#Is explanation size correlated with accuracy of answers?
library(dplyr);
library(stringi);

countCorrectAnswerByExplanationSize<-function(data){
  subsetTPTN_df = subset(data,select= c(Answer.explanationSize, TP, TN));
  CorrectAnswers = subsetTPTN_df$TP + subsetTPTN_df$TN;
  subsetTPTN_df = data.frame(subsetTPTN_df);
  subsetTPTN_df["CorrectAnswers"]<-CorrectAnswers;
  
  by_questions = group_by(subsetTPTN_df,Answer.explanationSize) ;
  summaryTable =   summaryTable = dplyr::summarize(by_questions,sum(CorrectAnswers));
  colnames(summaryTable) = c("Answer.explanationSize","CorrectAnswers");
  
  return(summaryTable);
}

#################################################################################
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf <- loadAnswers();

dataf$Answer.explanationSize <- stri_length(dataf$Answer.explanation);
summaryTable <- countCorrectAnswerByExplanationSize(data = dataf);

#Normality tests 
shapiro.test(summaryTable$Answer.explanationSize); 
#W = 0.82523, p-value < 2.2e-16 NOT NORMAL
qqnorm(y=summaryTable$Answer.explanationSize);
qqline(summaryTable$Answer.explanationSize);
#right skewed

shapiro.test(summaryTable$CorrectAnswers); 
#W = 0.89149, p-value = 0.0006019 NOT NORMAL
qqnorm(y=summaryTable$CorrectAnswers);
qqline(summaryTable$CorrectAnswers);
#Slight heavy tail 

output<- cor.test(summaryTable$Answer.explanationSize,summaryTable$CorrectAnswers,method="kendall",use="pairwise");
output$p.value
# 9.850517e-75 Significant

output$estimate
#-0.612358 negative correlation

ggplot(data=summaryTable, aes(summaryTable$Answer.explanationSize)) + geom_histogram()+
  ggtitle("Distribution of explanation sizes")+
  labs(x="Explanation size (characters)", 
       y="Frequency");

ggplot(data=summaryTable, aes(x=summaryTable$Answer.explanationSize,y=summaryTable$CorrectAnswers)) +
  geom_point()+  ggtitle("Distribution of Correct Answers by Explanation Size")+
  labs(x="Explanation size (characters)", 
       y="Number of correct answers");