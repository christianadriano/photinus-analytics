
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

#How many workers with score level X answered each question?
#i.e., what is the distribution of workers with score level x across questions?


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

level<-3;
tableScore_3 = countScoreLevel(level);
title<- paste("Fig.1 - Distribution of worker with score level",level,"across questions");
ggplot(tableScore_3,aes(x=scoreCount))+
  geom_histogram(binwidth = 0.2) +
  ggtitle(title) +
  labs(y="question count", x="worker score per question")
#Fig.1 shows that 20 questions had each 6 workers with score level-3, while 25 had each 5 workers with score level-3 

#The shape of the histograms resemble a normal distribuions
#Nonetheless, the data actually does not pass the Shapiro-Wilk normality test
shapiro.test(tableScore_3$scoreCount);

#This is also the case for the score levels 4 and 5 as shown in the 
#charts below.

level<- 4;
tableScore_4 = countScoreLevel(level);
title<- paste("Fig.2 - Distribution of worker with score level",level,"across questions");
ggplot(tableScore_4,aes(x=scoreCount))+
  geom_histogram(binwidth = 0.2) +
  ggtitle(title) +
  labs(y="question count", x="worker score per question")
 
shapiro.test(tableScore_4$scoreCount);

level<- 5;
tableScore_5 = countScoreLevel(level);
title<- paste("Fig.3 - Distribution of worker with score level",level,"across questions");
ggplot(tableScore_5,aes(x=scoreCount))+
  geom_histogram(binwidth = 0.2) +
  ggtitle(title) +
  labs(y="question count", x="worker score per question")
shapiro.test(tableScore_5$scoreCount);

#The distribution shapes are distinct, but we performed a non-parametric test to verify if
#the averages are also distinct.Since we are doing multiple comparisons, I applied 
#the Bonferroni adjustment. Since I will make 3 comparisons, the corrected confidence level 
#of is  1 - 0.05/3 =  0.0167 =0.983

wilcox.test(tableScore_3$scoreCount,tableScore_4$scoreCount,alternative = c("two.sided"), conf.level = 0.983);
wilcox.test(tableScore_3$scoreCount,tableScore_5$scoreCount,alternative = c("two.sided"), conf.level = 0.983);
wilcox.test(tableScore_5$scoreCount,tableScore_4$scoreCount,alternative = c("two.sided"), conf.level = 0.983);

#In all test, the null-hypothesis was rejected, which implies that the three data sets have different averages 
#considering a confidence interval of 98.3%.

#The previous analysis show that workers are not uniformly distributed in
#terms of test score. This might be an issue because questions that 
#were answered by workers with lower score, might have been overlooked.

#However, on average, worker score might still be more uniformly distributed
#across questions. This is what I will investigate next.

####We can also look at how questions were distributed in terms of woker average score 
#Data formatting
#1.concatenate the columns with count of levels per question
#2.compute the average score for each questions. I do that with a vector product of
#number of scores at level 3 x 3 + at level 4 X 4, at level 5 x 5
#after that I divide by 3 to take the average.

tableScores<-matrix(, nrow = 129, ncol = 4);
tableScores[,1]<-tableScore_3$QuestoinID;
tableScores[,2]<-tableScore_3$scoreCount;
tableScores[,3]<-tableScore_4$scoreCount;
tableScores[,4]<-tableScore_5$scoreCount;
colnames(tableScores)<-c("QuestionID","level_3","level_4","level_5");

scores<-c(3,4,5);
countVec<-c(1,1,1);

productScores<-(tableScores[,2:4]%*%scores); #vector product
countItemsVec<-(tableScores[,2:4]%*%countVec); #count number of scores per question
averageScores<- productScores / countItemsVec; #obtain the average score for each question

boxplot(averageScores)
summary(averageScores)
#We can see that the interval of average scores is reasonably tight, from 3.8 to 4.6
#We have two data points below 3.8, which I at least one outlier below 3.8

#Remove outliers
averageScores<-data.frame(averageScores);
averageScores <-averageScores[!averageScores$averageScores<3.8,]

#Plot distribution
averageScores<-data.frame(averageScores);
colnames(averageScores)<-"averageScores";

title<- paste("Fig.4 - Distribution of average worker score per question");
ggplot(averageScores,aes(x=averageScores))+
  geom_histogram(binwidth = 0.02) +
  ggtitle(title) +
  labs(y="score count", x="average worker score per question")

#Distribution is normal
shapiro.test(averageScores$averageScores);

# Since scores are discrete, 3, 4, or 5. We can calculate probability of a question having 
# an average score different than 4 (below or above 4).

scores<-averageScores$averageScores;
pop_sd <- sd(scores)*sqrt((length(scores)-1)/(length(scores)))
pop_mean <- mean(scores)
z <- (4 - pop_mean) / pop_sd  

cat("Probability of average score above 4=", round((1-pnorm(z))*100,2),"%");

#Conclusion
#Although workers are not uniformly distributed in terms of score, average worker score per 
#question follows a normal distribution and scores above 4 for 90% of questions, which represents 
#a small variation.
