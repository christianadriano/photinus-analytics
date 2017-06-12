
#Do bug-covering questions receive more answers from professional developers than other questions?
#Are these differences statistically significant?
library(dplyr);
#```{r Setup, echo=FALSE}
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");

#Bug covering question ID's
bugCoveringID<- c(1,4,10,14,20,23,30,32,55,56,57,58,59,72,73,77,84,92,95,97,102,104,115,119,123);

testRemove<- c(0,3);

dataf =loadAnswers();

##function to count the numer of workers with profession X for each question
countItem<- function(dataf, profession){
  subset_df <-subset(dataf,select= c(Worker.profession)); ##extract only the column Worker.profession)
  subset_df <- data.frame(subset_df); 
  
  if(length(profession)==0){
    itemCount<- rowSums(subset_df==profession); ##Row with ones when profession match, otherwise zero
  }
  else{
    itemCount<- 1; #c(1:dim(subset_df)[1]);
  }
    subset_df["itemCount"] <- itemCount;
  subset_df["Question.ID"] <- dataf$Question.ID;
   
 ## subset_df <-subset(subset_df,select= c(Question.ID,itemCount));##select only the rows with 
  question_by <- group_by(subset_df,Question.ID);
  summaryTable <- summarize(question_by,sum(itemCount));
  colnames(summaryTable)<-c("QuestionID","itemCount");
  
  return(summaryTable);
}

#``` 




### Actual analysis
#```{r analysis, echo=FALSE}



### Professional Developers
#Can we assert that bug covering questions have more professional developers answers on average? 

profession<-"Professional_Developer";
professionList<-countItem(dataf,profession);
bugCoveringList<- professionList[(professionList$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- professionList[!(professionList$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Normal W = 0.94579, p-value = 0.2011
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.94811, p-value = 0.0004707


wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1330, p-value = 0.8587

#Wilcox non-parametric test could not show that the average number of answers from professional developers are distinct in the two data sets.
#Howver, we should be cautions that the lack of evidence of an effect is not proof that of lack of effect.

### All non-students answers

students<- dataf[!(dataf$Worker.profession %in% c("Professional_Developer","Hobbyist")),];
non_students<- dataf[(dataf$Worker.profession %in% c("Professional_Developer","Hobbyist")),];

students<-countItem(students,"");
non_students<-countItem(non_students,"");

shapiro.test(students$itemCount); #Normal W = 0.94139, p-value = 2.807e-05
shapiro.test(non_students$itemCount); #Not normal W = 0.95102, p-value = 0.0001447

wilcox.test(students$itemCount,non_students$itemCount, alternative= "two.sided", paired=FALSE);
#data: students$itemCount and non_students$itemCount
#W = 1294.5, p-value < 2.2e-16 OPS!

#Wilcox non-parametric test showed that the average number of answers from students and non-students 

### non-students score = 100% (Professional_Developers, Hobbyists, and Others)

### worker score = 100%

### least difficult answers by worker score


