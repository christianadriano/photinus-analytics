
### The goal of this analysis
  # *How worker quality is distributed across questions? Worker quality was measured by three attributes. 
  # *Test score: workers received a score (zero to five) after performing a computer programming test used to filter 
  # out workers without enough programming knowledge. Workers who scored 2 or less were not allowed to participate.  
  # *Years of programming experience: Worker quality can also be measured by years of programming experience, which is an information self-declared by workers
  # befote they take any test. 
  # *Profession:worker profession is also an indicator of quality. Professions consisted of professional programmers, hobbyists,
  # graduate students, undergraduate students, and others.
  # 
  
library(dplyr);
library(ggplot2);

source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf =loadAnswers();


##by_questions<- group_by(subset_df,Question.ID) ;
countScoreLevel<- function(level){
  subset_df <-subset(dataf,select= c(Worker.score));
  subset_df <- data.frame(subset_df); 
  
  scoreCount<- rowSums(subset_df==level); 
  subset_df["scoreCount"] <- scoreCount;
  subset_df["Question.ID"] <- dataf$Question.ID;

  subset_df <-subset(subset_df,select= c(Question.ID,scoreCount));
  question_by <- group_by(subset_df,Question.ID);
  summaryTable <- summarize(question_by,sum(scoreCount));
  colnames(summaryTable)<-c("QuestoinID","scoreCount");
  
 
  return(summaryTable);
}

tableScore = countScoreLevel(3);
title<- cat("Distribution of worker score per question",3);

ggplot(summaryTable,aes(x=scoreCount))+
  geom_histogram(binwidth = 0.2) +
  ggtitle(title) +
  labs(y="question count", x="worker score per question")

tableScore = countScoreLevel(4);
tableScore = countScoreLevel(5);

#shape of the histogram resembles a normal distribuion

#Nonetheless, the data actually does not pass the Shapiro-Wilk normality test 
shapiro.test(tableScore$scoreCount);
