
#Do bug-covering questions receive more answers from professional developers than other questions?
#Are these differences statistically significant?
library(dplyr);
#```{r Setup, echo=FALSE}
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");

#Bug covering question ID's
bugCoveringID<- c(1,4,10,14,20,23,30,32,55,56,57,58,59,72,73,77,84,92,95,97,102,104,115,119,123);

dataf =loadAnswers();

##function to count the numer of workers with profession X for each question
#Add two columns to the data that is passed (QuestionID, itemCount)
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

#Wilcox non-parametric test could not show that the average number of answers to bug-covering and non-bug covering 
#questions are distinct. However, we should be cautions that the lack of evidence of an effect is not proof that of lack of effect.

############################################################################
### All non-students answers

non_students<- dataf[!(dataf$Worker.profession %in% c("Professional_Developer","Hobbyist","Other")),];

non_students<-countItem(non_students,""); 

bugCoveringList<- non_students[(non_students$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- non_students[!(non_students$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Not Normal W = 0.90395, p-value = 0.02239
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.94641, p-value = 0.0003637


wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1196.5, p-value = 0.5333

#Wilcox non-parametric test could not show that the average number of answers to bug-covering and non-bug covering 
#questions are distinct. However, we should be cautions that the lack of evidence of an effect is not proof that of lack of effect.

##############################################################################
## PROFESSION ################################################################
############################################################################
### All non-students answer
#OK, filter does not cause bias

non_students<- dataf[!(dataf$Worker.profession %in% c("Professional_Developer","Hobbyist","Other")),];
non_students<-countItem(non_students,""); 

bugCoveringList<- non_students[(non_students$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- non_students[!(non_students$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Not Normal W = 0.90395, p-value = 0.02239
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.94641, p-value = 0.0003637


wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1196.5, p-value = 0.5333

#Wilcox non-parametric test could not show that the average number of answers to bug-covering and non-bug covering 
#questions are distinct. However, we should be cautions that the lack of evidence of an effect is not proof that of lack of effect.

############################
### FILTER-OUT Undergraduate_Student,Graduate_Student, hobbyists, others 
#OK, filter does not cause bias

professionList<- dataf[!(dataf$Worker.profession %in% c("Undergraduate_Student","Graduate_Student","Hobbyist","Other")),];

professionList<-countItem(professionList,""); 

bugCoveringList<- professionList[(professionList$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- professionList[!(professionList$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Not Normal W = 0.94579, p-value = 0.2011
shapiro.test(not_bugCoveringList$itemCount); #Not normalW = 0.94811, p-value = 0.0004707


wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1330, p-value = 0.8587

#Wilcox non-parametric test could not show that the average number of answers to bug-covering and non-bug covering 
#questions are distinct. However, we should be cautions that the lack of evidence of an effect is not proof that of lack of effect.

############################
### FILTER-OUT Graduate_Student, hobbyists, others 
#OK, filter does not cause bias

professionList<- dataf[!(dataf$Worker.profession %in% c("Graduate_Student","Hobbyist","Other")),];

professionList<-countItem(professionList,""); 

bugCoveringList<- professionList[(professionList$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- professionList[!(professionList$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Normal W = 0.94106, p-value = 0.1566
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.9589, p-value = 0.002643


wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1337.5, p-value = 0.8231

#Wilcox non-parametric test could not show that the average number of answers to bug-covering and non-bug covering 
#questions are distinct. However, we should be cautions that the lack of evidence of an effect is not proof that of lack of effect.

############################
### FILTER-OUT Undergraduate_Student,Graduate_Student, others 
#OK, filter does not cause bias

professionList<- dataf[!(dataf$Worker.profession %in% c("Undergraduate_Student","Graduate_Student","Other")),];

professionList<-countItem(professionList,""); 

bugCoveringList<- professionList[(professionList$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- professionList[!(professionList$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Not Normal W = 0.90256, p-value = 0.02086
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.95576, p-value = 0.001575


wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1477.5, p-value = 0.2851

#Wilcox non-parametric test could not show that the average number of answers to bug-covering and non-bug covering 
#questions are distinct. However, we should be cautions that the lack of evidence of an effect is not proof that of lack of effect.




##############################################################################
## SCORE #####################################################
############################################################################

### worker score = 100%
#Not OK, filter injected bias

workers_100<- dataf[(dataf$Worker.score == 5),];

workers_100<-countItem(workers_100,""); 

bugCoveringList<- workers_100[(workers_100$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- workers_100[!(workers_100$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Not Normal W = 0.88385, p-value = 0.008305
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.95902, p-value = 0.002696

mean(bugCoveringList$itemCount)
#9.48
mean(not_bugCoveringList$itemCount)
#8.5

wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE, conf.int = TRUE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1643, p-value = 0.03867
#alternative hypothesis: true location shift is not equal to 0
#95 percent confidence interval:
#  3.075891e-05 1.999998e+00
#sample estimates:
#  difference in location 
#0.9999684 

#Wilcox non-parametric test SHOWED that the average number of answers for bug covering and non-bug covering are distinct with a 95% confidence interval.
#Moreover, bug-covering received more answers in average. This unbalance implies that we cannot trust that the filter is actually fair. However, the difference
#between the two median number of answers is small (approximately one answer of difference). 
#Therefore, we kept the outcome of the filter but we marked it with a star.

#####################################
### worker score = 100% or 80%
#Not OK, filter injected bias 

workers_100_80<- dataf[(dataf$Worker.score %in% c(5,4)),];

workers_100_80<-countItem(workers_100_80,""); 

bugCoveringList<- workers_100_80[(workers_100_80$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- workers_100_80[!(workers_100_80$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Normal W = 0.93497, p-value = 0.1132
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.93347, p-value = 5.672e-05

mean(bugCoveringList$itemCount)
#15
mean(not_bugCoveringList$itemCount)
#14.58654

wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE, conf.int = TRUE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1360.5, p-value = 0.7175
#alternative hypothesis: true location shift is not equal to 0
#95 percent confidence interval:
#-0.999947  1.000040
#sample estimates:
#  difference in location 
#5.961757e-06 

#Wilcox non-parametric test COULD NOT SHOW whether the average number of answers for bug covering and non-bug covering are distinct with a 95% confidence interval.

#########################################################


#####################################
### worker score = 80% or 60%
#Not OK, filter injected bias

workers_80_60<- dataf[(dataf$Worker.score %in% c(4,3)),];

workers_80_60<-countItem(workers_80_60,""); 

bugCoveringList<- workers_80_60[(workers_80_60$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- workers_80_60[!(workers_80_60$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Not Normal W = 0.90188, p-value = 0.02016
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.96726, p-value = 0.01121

mean(bugCoveringList$itemCount)
#10.28
mean(not_bugCoveringList$itemCount)
#11.33654

wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE, conf.int = TRUE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 924.5, p-value = 0.0238
#alternative hypothesis: true location shift is not equal to 0
#95 percent confidence interval:
#  -1.999977e+00 -4.944597e-05
#sample estimates:
#  difference in location -1.000029 

#Wilcox non-parametric test SHOWED that the average number of answers for bug covering and non-bug covering are distinct with a 95% confidence interval.
#Moreover, bug-covering received more answers in average. This unbalance implies that we cannot trust that the filter is actually fair. However, the difference
#between the two median number of answers is small (approximately one answer of difference). 
#Therefore, we kept the outcome of the filter but we marked it with a star.


#########################################################


##############################################################################
## PROFESSION AND SCORE #####################################################
############################################################################
#Not OK, filter injected bias

### non-students score = 100% (Professional_Developers, Hobbyists, and Others)
non_students<- dataf[(dataf$Worker.profession %in% c("Professional_Developer","Hobbyist","Other")),];
non_students<- non_students[(non_students$Worker.score == 5),];

non_students<-countItem(non_students,""); 

bugCoveringList<- non_students[(non_students$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- non_students[!(non_students$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Normal W = 0.95015, p-value = 0.2527
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.96348, p-value = 0.005757

mean(bugCoveringList$itemCount)
#7.36
mean(not_bugCoveringList$itemCount)
#6.27

wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE, conf.int = TRUE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1715, p-value = 0.01214
#alternative hypothesis: true location shift is not equal to 0
#95 percent confidence interval:
#  3.068510e-06 1.999979e+00
#sample estimates difference in location = 1.000018 

#Wilcox non-parametric test SHOWED that the average number of answers for bug covering and non-bug covering are distinct with a 95% confidence interval.
#Moreover, bug-covering received more answers in average. This unbalance implies that we cannot trust that the filter is actually fair, therefore the 
#outcome of this filter cannot be un For this reason, we cannot 

############################################################################
#Not OK, filter injected bias

#Filter OUT student with 60% score

removedStudent_60<- dataf[!((dataf$Worker.profession %in% c("Graduate_Student","Undergraduate_Student")) & dataf$Worker.score %in% c(3)),];

#Counts the number of occurrences of each questions in the dataset.
removedStudent_60<-countItem(removedStudent_60,""); 

bugCoveringList<- removedStudent_60[(removedStudent_60$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- removedStudent_60[!(removedStudent_60$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Normal W = 0.92527, p-value = 0.06762
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.93354, p-value = 5.723e-05

mean(bugCoveringList$itemCount)
#17.4
mean(not_bugCoveringList$itemCount)
#17.63

wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE, conf.int = TRUE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1173.5, p-value = 0.4415
#alternative hypothesis: true location shift is not equal to 0
#95 percent confidence interval:
#  -0.9999826279  0.0000290348
#sample estimates difference in location = -2.867395e-05 

#Wilcox non-parametric test COULD NOT SHOW that the average number of answers for bug covering and 
#non-bug covering are distinct with a 95% confidence interval.

##############################################
### FILTER OUT (all students) && (non-students below 60%)
##leave only non-students with score 100% and 80%
#OK, filter does not cause bias

non_students<- dataf[!(dataf$Worker.profession %in% c("Graduate_Student","Undergraduate_Student")),]

non_student_100_80<- non_students[(non_students$Worker.score %in% c(4,5)),];

non_student_100_80<-countItem(non_student_100_80,""); 

bugCoveringList<- non_student_100_80[(non_student_100_80$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- non_student_100_80[!(non_student_100_80$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Normal W = 0.93489, p-value = 0.1128
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.96072, p-value = 0.003591

mean(bugCoveringList$itemCount) #11.28
mean(not_bugCoveringList$itemCount) #10.49

wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE, conf.int = TRUE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1547, p-value = 0.1384

#Wilcox non-parametric test COULD NOT SHOW that the average number of answers for bug covering and non-bug covering are distinct with a 95% confidence interval.

##########################################################################
####### SCORE and DIFFICULTY
##########################################################################

### least difficult answers by worker score

#Evaluate filtering by each worker score separately. Filtering by difficulty levels doesn't insert bias because difficulty 
#is not an independent variable such as workers. Independent variables could be randomly allocated, while dependent variables cannot
#be randomly allocated. The assumption is that successive experiments with workers randomly allocated to tasks might present 
#concentration of more skilled workers on certain questions. However, difficulty levels attributed to question will not 
#vary across workers with different levels of expertise. In anyway, we still tested this filter with respect to bias.

##############################################
### FILTER OUT ([difficulty 5] and [all scores])
#OK, filter does not cause bias

filteredSet<- dataf[!(dataf$Answer.difficulty %in% c(5)),]

filteredSet<-countItem(filteredSet,""); 

bugCoveringList<- filteredSet[(filteredSet$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- filteredSet[!(filteredSet$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Not normal W = 0.79116, p-value = 0.0001625
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.95419, p-value = 0.001221

mean(bugCoveringList$itemCount) #16.4
mean(not_bugCoveringList$itemCount) #17.02885

wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE, conf.int = TRUE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1088, p-value = 0.2009

#Wilcox non-parametric test COULD NOT SHOW that the average number of answers for bug covering and non-bug covering are distinct with a 95% confidence interval.

#######################################################
### FILTER OUT ([difficulty 5] and [scores 80% 60%])
#OK, filter does not cause bias

filteredSet<- dataf[!((dataf$Answer.difficulty %in% c(5)) & (dataf$Worker.score %in% c(4,3))),]

filteredSet<-countItem(filteredSet,""); 

bugCoveringList<- filteredSet[(filteredSet$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- filteredSet[!(filteredSet$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Not normal W = 0.77357, p-value = 8.501e-05
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.91952, p-value = 9.219e-06

mean(bugCoveringList$itemCount) #17.92
mean(not_bugCoveringList$itemCount) #18.06731

wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE, conf.int = TRUE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1228, p-value = 0.6628

#Wilcox non-parametric test COULD NOT SHOW that the average number of answers for bug covering and non-bug covering are distinct with a 95% confidence interval.

#######################################################
### FILTER OUT ([difficulty 5] and [scores 80%]) U ([difficulty 5,4] and [scores 60%])
#OK, filter does not cause bias

filteredSet<- dataf[!((dataf$Answer.difficulty %in% c(5)) & (dataf$Worker.score %in% c(4))),]

filteredSet<- filteredSet[!((filteredSet$Answer.difficulty %in% c(5,4)) & (filteredSet$Worker.score %in% c(3))),]

filteredSet<-countItem(filteredSet,""); 

bugCoveringList<- filteredSet[(filteredSet$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- filteredSet[!(filteredSet$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Not normal W = 0.92785, p-value = 0.07752
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.95128, p-value = 0.0007699

mean(bugCoveringList$itemCount) #16.76
mean(not_bugCoveringList$itemCount) #16.78846

wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE, conf.int = TRUE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1268, p-value = 0.8492

#Wilcox non-parametric test COULD NOT SHOW that the average number of answers for bug covering and non-bug covering are distinct with a 95% confidence interval.

#######################################################
### FILTER OUT ([difficulty 5,4] and [scores 80%,60%])
#OK, filter does not cause bias

filteredSet<- dataf[!((dataf$Answer.difficulty %in% c(5,4)) & (dataf$Worker.score %in% c(4,3))),]

filteredSet<-countItem(filteredSet,""); 

bugCoveringList<- filteredSet[(filteredSet$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- filteredSet[!(filteredSet$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #Not normal W = 0.94615, p-value = 0.205
shapiro.test(not_bugCoveringList$itemCount); #Not normal W = 0.97251, p-value = 0.02903

mean(bugCoveringList$itemCount) #15.48
mean(not_bugCoveringList$itemCount) #15.47115

wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE, conf.int = TRUE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1286.5, p-value = 0.9377

#Wilcox non-parametric test COULD NOT SHOW that the average number of answers for bug covering and non-bug covering are distinct with a 95% confidence interval.

################################################################
## DIFFICULTY and PROFESSION
################################################################

#######################################################
### FILTER OUT ([difficulty 5,4,3] and [profession 80%,60%])
#OK, filter does not cause bias

filteredSet<- dataf[!((dataf$Answer.difficulty %in% c(5,4,3)) & (dataf$Worker.profession %in% c("Professional_Programmer", "Hobbyist", "Other"))),]

filteredSet<- filteredSet[!((filteredSet$Worker.profession %in% c("Graduate_Student", "Undergraduate_Student"))),]

filteredSet<-countItem(filteredSet,""); 

bugCoveringList<- filteredSet[(filteredSet$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- filteredSet[!(filteredSet$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #normal W = 0.94012, p-value = 0.149
shapiro.test(not_bugCoveringList$itemCount); # normal W = 0.97652, p-value = 0.06127

mean(bugCoveringList$itemCount) #9.52
mean(not_bugCoveringList$itemCount) #9.605769

t.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE, conf.int = TRUE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#t = -0.171, df = 35.051, p-value = 0.8652

#Wilcox non-parametric test COULD NOT SHOW that the average number of answers for bug covering and non-bug covering are distinct
#with a 95% confidence interval.

#######################################################
### FILTER OUT ([difficulty 5] and [profession Graduate_Student]) U( [difficulty 5,4] and [Undergrad])
#OK, filter does not cause bias

filteredSet<- dataf[!((dataf$Answer.difficulty %in% c(5)) & (dataf$Worker.profession %in% c("Graduate_Student"))),]

filteredSet<- filteredSet[!((filteredSet$Answer.difficulty %in% c(5,4)) & (filteredSet$Worker.profession %in% c("Undergraduate_Student"))),]

filteredSet<-countItem(filteredSet,""); 

bugCoveringList<- filteredSet[(filteredSet$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- filteredSet[!(filteredSet$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #NOT normal W = 0.78078, p-value = 0.0001105
shapiro.test(not_bugCoveringList$itemCount); #NOT normal W = 0.89938, p-value = 8.781e-07

mean(bugCoveringList$itemCount) #17.92
mean(not_bugCoveringList$itemCount) #18.13462

wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE, conf.int = TRUE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1170.5, p-value = 0.4286

#Wilcox non-parametric test COULD NOT SHOW that the average number of answers for bug covering and non-bug covering are distinct
#with a 95% confidence interval.


#######################################################
### FILTER OUT ([difficulty 5,4,3] and [profession Graduate_Student, Undergrad])
#OK, filter does not cause bias

filteredSet<- dataf[!((dataf$Answer.difficulty %in% c(5,4,3)) & (dataf$Worker.profession %in% c("Graduate_Student","Undergraduate_Student"))),]

filteredSet<-countItem(filteredSet,""); 

bugCoveringList<- filteredSet[(filteredSet$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- filteredSet[!(filteredSet$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #normal W = 0.96139, p-value = 0.4428
shapiro.test(not_bugCoveringList$itemCount); #NOT normal W = 0.92658, p-value = 2.262e-05

mean(bugCoveringList$itemCount) #15.36
mean(not_bugCoveringList$itemCount) #15.75

wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE, conf.int = TRUE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1130, p-value = 0.3053

#Wilcox non-parametric test COULD NOT SHOW that the average number of answers for bug covering and non-bug covering are distinct
#with a 95% confidence interval.

#######################################################
### FILTER OUT ([difficulty 5,4] and [profession Graduate_Student, Undergrad])
#OK, filter does not cause bias

filteredSet<- dataf[!((dataf$Answer.difficulty %in% c(5,4)) & (dataf$Worker.profession %in% c("Graduate_Student","Undergraduate_Student"))),]

filteredSet<-countItem(filteredSet,""); 

bugCoveringList<- filteredSet[(filteredSet$QuestionID %in% bugCoveringID),];
not_bugCoveringList<- filteredSet[!(filteredSet$QuestionID %in% bugCoveringID),];

shapiro.test(bugCoveringList$itemCount); #NOT normalW = 0.84392, p-value = 0.001353
shapiro.test(not_bugCoveringList$itemCount); #NOT normal W = 0.91797, p-value = 7.613e-06

mean(bugCoveringList$itemCount) #17.32
mean(not_bugCoveringList$itemCount) #17.52885

wilcox.test(bugCoveringList$itemCount,not_bugCoveringList$itemCount, alternative= "two.sided", paired=FALSE, conf.int = TRUE);
#data:  bugCoveringList$itemCount and not_bugCoveringList$itemCount
#W = 1219.5, p-value = 0.6271

#Wilcox non-parametric test COULD NOT SHOW that the average number of answers for bug covering and non-bug covering are distinct
#with a 95% confidence interval.