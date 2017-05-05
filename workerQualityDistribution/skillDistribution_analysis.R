##
# Is worker quality uniformly distributed across questions?
## This is important so when we filter by quality of worker, we do not favor some questions over the others.

##ACCURACY DISTRIBUTION ACROSS QUESTIONS
# First we compute how skills are distributed over the 129 questions

source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf =loadAnswers();

subsetTPTN_df <-subset(dataf,select= c(Question.ID, TP, TN));
TrueCount <- subsetTPTN_df$TP + subsetTPTN_df$TN;
subsetTPTN_df <- data.frame(subsetTPTN_df);
subsetTPTN_df["TrueCount"]<-TrueCount;

install.packages('dplyr',, dependencies = TRUE);
library(dplyr);
by_questions<- group_by(subsetTPTN_df,Question.ID) ;
TrueCountTable <- summarize(by_questions,sum(TrueCount));
colnames(TrueCountTable)<-c("QuestoinID","TrueCount");

library(ggplot2);
ggplot(TrueCountTable,aes(x=TrueCount)) + geom_histogram(binwidth = 0.5);
#shape of the histogram resembles a normal distribuion

#Nonetheless, the data actually does not pass the Shapiro-Wilk normality test 
shapiro.test(TrueCountTable$TrueCount);

##SKILL DISTRIBUTION ACROSS QUESTIONS


##PROFESSION DISTRIBUTION ACROSS QUESTIONS