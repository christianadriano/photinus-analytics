##
# Is worker quality uniformly distributed across questions?
## This is important so when we filter by quality of worker, we do not favor some questions over the others.

##ACCURACY DISTRIBUTION ACROSS QUESTIONS
# First we compute how skills are distributed over the 129 questions
install.packages('dplyr',, dependencies = TRUE);
library(dplyr);
library(ggplot2);

source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf =loadAnswers();

subsetTPTN_df <-subset(dataf,select= c(Question.ID, TP, TN));
CorrectAnswers <- subsetTPTN_df$TP + subsetTPTN_df$TN;
subsetTPTN_df <- data.frame(subsetTPTN_df);
subsetTPTN_df["CorrectAnswers"]<-CorrectAnswers;

by_questions<- group_by(subsetTPTN_df,Question.ID) ;
summaryTable <- summarize(by_questions,sum(CorrectAnswers));
colnames(summaryTable)<-c("QuestoinID","CorrectAnswers");


ggplot(summaryTable,aes(x=CorrectAnswers))+ geom_histogram(binwidth = 0.25) 


+ ggtitle("Distribution of correct answers per question - counts both True Positives and True Negatives")
+ labs(y="count", x="correct answers per question");

#shape of the histogram resembles a normal distribuion

#Nonetheless, the data actually does not pass the Shapiro-Wilk normality test 
shapiro.test(TrueCountTable$TrueCount);

##SKILL DISTRIBUTION ACROSS QUESTIONS


##PROFESSION DISTRIBUTION ACROSS QUESTIONS