#
# Compute correlation among difficulty and accuracy
#

#Setup
install.packages("ggplot2");
library("ggplot2");
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//correlationGraphs//multiplot.R");

loadAnswers<- function(fileName){
  
  setwd("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//correlationGraphs//");
  data_all <- read.csv(fileName,header = TRUE,sep=",");
  dataf = data.frame(data_all);
  return(dataf);
}

dataf<- loadAnswers("answerList_photinus_data.csv");
#Check if total answers is 2580
length(dataf$TP)




#Select TP, TN, FP, FN for the item
selectOutcomes <- function(df,lineNumbers){
  dataf_difficulty <- df [(lineNumbers) ,];
  TP <- sum(dataf_difficulty$TP);
  TN <- sum(dataf_difficulty$TN);
  FP <- sum(dataf_difficulty$FP);
  FN <- sum(dataf_difficulty$FN);
  outcome.df <- data.frame(TP,TN,FP,FN);
  return (outcome.df);
}

#Compute accuracy 
computeAccuracy<- function(df){
  return((df$TP+df$TN)/(df$TP+df$TN+df$FP+df$FN));
}

#Compute precision
computePrecision<- function(df){
  result<-(df$TP)/(df$TP+df$FP);
  if(is.nan(sum(result))){
    index<- is.nan(result);
    result[index]<-0;
  }
  return(result);
}

#Compute recall
computeRecall<- function(df){
  return((df$TP)/(df$TP+df$FN));
}

## Computes a table with statistics each level of difficulty
computeStats<- function(df,selection, filterName){
  
  dataframe <- data.frame(Difficulty=double(),Accuracy=double(), Precision=double(), Recall=double(), Answers=double() );
  
  for(i in 1:5){
    outcomesDF<- selectOutcomes(df,selection==i);
    dataframe[i,1]<-i;
    dataframe[i,2]<-computeAccuracy(outcomesDF);
    dataframe[i,3]<-computePrecision(outcomesDF);
    dataframe[i,4]<-computeRecall(outcomesDF);
    dataframe[i,5]<-sum(outcomesDF[1,]);
  }
  print(filterName);
  print(dataframe);
  message("Total answers:" , sum(dataframe$Answers));
  
  return(dataframe);
}

## Builds plots with multiple lines
plotMultiLine <- function(stats,title){
  
  multiplot <- ggplot(stats, aes(x=Difficulty),fill="Metrics") +
    geom_line(aes(y = Accuracy, colour="Accuracy")) + 
    geom_line(aes(y = Recall, colour = "Recall")) +
    geom_line(aes(y = Precision, colour = "Precision")) +
    ggtitle(title) +
    ylab(label="Metrics") + 
    xlab("difficulty") +
    scale_colour_manual("", 
                        breaks = c("Accuracy" ,"Recall","Precision"),
                        values = c("Accuracy"="blue","Recall"="black","Precision"="red"))+
    scale_y_continuous(limits = c(0, 1));
  
  return(multiplot);
}

plotSingleLine <- function(stats,title){
  
  singlePlot <- ggplot(stats, aes(x=Difficulty),fill="Metrics") +
    geom_line(aes(y = Accuracy, colour="Accuracy")) + 
    ggtitle(title) +
    ylab(label="Metrics") + 
    xlab("difficulty") +
    scale_colour_manual("", 
                        breaks = c("Accuracy"),
                        values = c("Accuracy"="blue"))+
    scale_y_continuous(limits = c(0, 1));
  return(singlePlot);
}

################################################################################
##### Analysis by Answer option (YES, NO)

## ALL answers
stats1<- computeStats(dataf,dataf$Answer.difficulty,"All Answers - difficulty");

## Only YES's
dataYES <- dataf [(dataf$Answer.option=="YES") ,];
stats2<- computeStats(dataYES,dataYES$Answer.difficulty,"Only YES Answers - difficulty");

## Only NO's
dataNO <- dataf [(dataf$Answer.option=="NO") ,];
stats3<- computeStats(dataNO,dataNO$Answer.difficulty,"Only NO Answers - difficulty");

#Plotting
# breaks =c("Accuracy" ,"Recall","Precision")
p1<- plotMultiLine(stats1,"All answers");
p2<- plotSingleLine(stats2,"Only Yes answers");
p3<- plotSingleLine(stats3,"Only No answers");
multiplot(p1, p2, p3, cols=2);


################################################################################

#### WORKER PROFESSION
## Only PROFESSIONAL_DEVELOPERS
dataProf <- dataf [(dataf$Worker.profession=="Professional_Developer") ,];
stats1<- computeStats(dataProf,dataProf$Answer.difficulty,"Only PROFESSIONAL_DEVELOPERS - difficulty");

## Only HOBBYIST´S
dataProf <- dataf [(dataf$Worker.profession=="Hobbyist") ,];
stats2<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HOBBYIST - difficulty");

## Only GRADUATE_STUDENT´S
dataProf <- dataf [(dataf$Worker.profession=="Graduate_Student") ,];
stats3<- computeStats(dataProf,dataProf$Answer.difficulty,"Only GRADUATE_STUDENT - difficulty");

## Only Hobbyist´S
dataProf <- dataf [(dataf$Worker.profession=="Hobbyist") ,];
stats4<- computeStats(dataProf,dataProf$Answer.difficulty,"Only Hobbyist - difficulty");

## Only OTHER´S
dataProf <- dataf [(dataf$Worker.profession=="Other") ,];
stats5<- computeStats(dataProf,dataProf$Answer.difficulty,"Only OTHER - difficulty");

# breaks =c("Accuracy" ,"Recall","Precision")
p1<- plotMultiLine(stats1,"Professional Developers");
p2<- plotMultiLine(stats2,"Hobbyists");
p3<- plotMultiLine(stats3,"Graduate students");
p4<- plotMultiLine(stats4,"Undergraduate students");
p5<- plotMultiLine(stats5,"Others");
multiplot(p1, p2, p3, p4, p5, cols=3);


################################################################################

#### JAVA METHOD
dataProf <- dataf [(dataf$FailingMethod =="HIT01_8") ,];
stats1<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT01_8 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT02_24") ,];
stats2<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT02_24 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT03_6") ,];
stats3<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT03_6 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT04_7") ,];
stats4<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT04_7 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT05_35") ,];
stats5<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT05_35 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT06_51") ,];
stats6<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT06_51 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT07_33") ,];
stats7<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT07_33 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT08_54") ,];
stats8<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT08_54 - difficulty");

# breaks =c("Accuracy" ,"Recall","Precision")

p2<- plotSingleLine(stats2,"HIT02_24");
p5<- plotSingleLine(stats5,"HIT05_35");
p7<- plotSingleLine(stats7,"HIT07_33");
p1<- plotSingleLine(stats1,"HIT01_8");
p3<- plotSingleLine(stats3,"HIT03_6");
p6<- plotSingleLine(stats6,"HIT06_51");
p8<- plotSingleLine(stats8,"HIT08_54");
p4<- plotSingleLine(stats4,"HIT04_7");

multiplot(p2, p5, p7, p1,cols=4);

multiplot(p3, p6, p8, p4, cols=4);


################################################################################
# Java METHOD and PROFESSIONAL DEVELOPERS

dataProf <- dataf [(dataf$FailingMethod =="HIT01_8") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats1<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT01_8 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT02_24") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats2<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT02_24 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT03_6") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats3<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT03_6 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT04_7") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats4<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT04_7 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT05_35") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats5<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT05_35 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT06_51") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats6<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT06_51 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT07_33") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats7<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT07_33 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT08_54") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats8<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT08_54 - difficulty");

p2<- plotSingleLine(stats2,"HIT02_24");
p5<- plotSingleLine(stats5,"HIT05_35");
p7<- plotSingleLine(stats7,"HIT07_33");
p1<- plotSingleLine(stats1,"HIT01_8");
p3<- plotSingleLine(stats3,"HIT03_6");
p6<- plotSingleLine(stats6,"HIT06_51");
p8<- plotSingleLine(stats8,"HIT08_54");
p4<- plotSingleLine(stats4,"HIT04_7");

multiplot(p2, p5, p7, p1,cols=2);

multiplot(p3, p6, p8, p4, cols=2);

################################################################################
# Java METHOD and UNDERGRADS

dataProf <- dataf [(dataf$FailingMethod =="HIT01_8") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats1<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT01_8 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT02_24") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats2<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT02_24 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT03_6") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats3<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT03_6 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT04_7") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats4<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT04_7 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT05_35") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats5<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT05_35 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT06_51") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats6<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT06_51 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT07_33") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats7<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT07_33 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT08_54") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats8<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT08_54 - difficulty");

p2<- plotSingleLine(stats2,"HIT02_24");
p5<- plotSingleLine(stats5,"HIT05_35");
p7<- plotSingleLine(stats7,"HIT07_33");
p1<- plotSingleLine(stats1,"HIT01_8");
p3<- plotSingleLine(stats3,"HIT03_6");
p6<- plotSingleLine(stats6,"HIT06_51");
p8<- plotSingleLine(stats8,"HIT08_54");
p4<- plotSingleLine(stats4,"HIT04_7");

multiplot(p2, p5, p7, p1, cols=2);

multiplot(p3, p6, p8, p4, cols=2);

################################################################################
# Java METHOD and UNDERGRADS and YES ANSWER OPTION

dataProf <- dataf [(dataf$FailingMethod =="HIT01_8") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats1<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT01_8 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT02_24") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats2<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT02_24 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT03_6") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats3<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT03_6 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT04_7") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats4<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT04_7 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT05_35") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats5<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT05_35 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT06_51") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats6<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT06_51 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT07_33") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats7<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT07_33 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT08_54") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats8<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT08_54 - difficulty");

p2<- plotSingleLine(stats2,"HIT02_24");
p5<- plotSingleLine(stats5,"HIT05_35");
p7<- plotSingleLine(stats7,"HIT07_33");
p1<- plotSingleLine(stats1,"HIT01_8");
p3<- plotSingleLine(stats3,"HIT03_6");
p6<- plotSingleLine(stats6,"HIT06_51");
p8<- plotSingleLine(stats8,"HIT08_54");
p4<- plotSingleLine(stats4,"HIT04_7");

multiplot(p2, p5, p7, p1,cols=4);

multiplot(p3, p6, p8, p4, cols=4);
############################################################################

################################################################################
# Java METHOD and PROFESSIONALS and YES ANSWER OPTION

dataProf <- dataf [(dataf$FailingMethod =="HIT01_8") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats1<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT01_8 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT02_24") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats2<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT02_24 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT03_6") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats3<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT03_6 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT04_7") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats4<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT04_7 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT05_35") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats5<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT05_35 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT06_51") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats6<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT06_51 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT07_33") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats7<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT07_33 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT08_54") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats8<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT08_54 - difficulty");

p2<- plotSingleLine(stats2,"HIT02_24");
p5<- plotSingleLine(stats5,"HIT05_35");
p7<- plotSingleLine(stats7,"HIT07_33");
p1<- plotSingleLine(stats1,"HIT01_8");
p3<- plotSingleLine(stats3,"HIT03_6");
p6<- plotSingleLine(stats6,"HIT06_51");
p8<- plotSingleLine(stats8,"HIT08_54");
p4<- plotSingleLine(stats4,"HIT04_7");

multiplot(p2, p5, p7, p1,cols=4);

multiplot(p3, p6, p8, p4, cols=4);

################################################################################
# Java METHOD and UNDERGRADS and NO ANSWER OPTION

dataProf <- dataf [(dataf$FailingMethod =="HIT01_8") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats1<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT01_8 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT02_24") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats2<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT02_24 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT03_6") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats3<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT03_6 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT04_7") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats4<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT04_7 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT05_35") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats5<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT05_35 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT06_51") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats6<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT06_51 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT07_33") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats7<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT07_33 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT08_54") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats8<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT08_54 - difficulty");

p2<- plotSingleLine(stats2,"HIT02_24");
p5<- plotSingleLine(stats5,"HIT05_35");
p7<- plotSingleLine(stats7,"HIT07_33");
p1<- plotSingleLine(stats1,"HIT01_8");
p3<- plotSingleLine(stats3,"HIT03_6");
p6<- plotSingleLine(stats6,"HIT06_51");
p8<- plotSingleLine(stats8,"HIT08_54");
p4<- plotSingleLine(stats4,"HIT04_7");

multiplot(p2, p5, p7, p1,cols=4);

multiplot(p3, p6, p8, p4, cols=4);

################################################################################
# Java METHOD and PROFESSIONALS and NO ANSWER OPTION

dataProf <- dataf [(dataf$FailingMethod =="HIT01_8") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats1<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT01_8 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT02_24") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats2<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT02_24 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT03_6") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats3<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT03_6 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT04_7") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats4<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT04_7 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT05_35") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats5<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT05_35 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT06_51") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats6<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT06_51 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT07_33") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats7<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT07_33 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT08_54") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats8<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT08_54 - difficulty");

p2<- plotSingleLine(stats2,"HIT02_24");
p5<- plotSingleLine(stats5,"HIT05_35");
p7<- plotSingleLine(stats7,"HIT07_33");
p1<- plotSingleLine(stats1,"HIT01_8");
p3<- plotSingleLine(stats3,"HIT03_6");
p6<- plotSingleLine(stats6,"HIT06_51");
p8<- plotSingleLine(stats8,"HIT08_54");
p4<- plotSingleLine(stats4,"HIT04_7");

multiplot(p2, p5, p7, p1,cols=4);

multiplot(p3, p6, p8, p4, cols=4);

################################################################################
# Java METHOD and GRADUATES and NO ANSWER OPTION

dataProf <- dataf [(dataf$FailingMethod =="HIT01_8") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Graduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats1<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT01_8 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT02_24") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Graduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats2<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT02_24 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT03_6") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Graduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats3<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT03_6 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT04_7") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Graduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats4<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT04_7 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT05_35") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Graduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats5<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT05_35 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT06_51") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Graduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats6<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT06_51 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT07_33") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Graduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats7<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT07_33 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT08_54") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Graduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats8<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT08_54 - difficulty");

p2<- plotSingleLine(stats2,"HIT02_24");
p5<- plotSingleLine(stats5,"HIT05_35");
p7<- plotSingleLine(stats7,"HIT07_33");
p1<- plotSingleLine(stats1,"HIT01_8");
p3<- plotSingleLine(stats3,"HIT03_6");
p6<- plotSingleLine(stats6,"HIT06_51");
p8<- plotSingleLine(stats8,"HIT08_54");
p4<- plotSingleLine(stats4,"HIT04_7");

multiplot(p2, p5, p7, p1,cols=4);

multiplot(p3, p6, p8, p4, cols=4);



################################################################################
# Java METHOD and OTHER and NO ANSWER OPTION

dataProf <- dataf [(dataf$FailingMethod =="HIT01_8") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Other"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats1<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT01_8 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT02_24") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Other"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats2<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT02_24 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT03_6") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Other"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats3<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT03_6 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT04_7") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Other"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats4<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT04_7 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT05_35") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Other"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats5<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT05_35 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT06_51") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Other"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats6<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT06_51 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT07_33") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Other"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats7<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT07_33 - difficulty");

dataProf <- dataf [(dataf$FailingMethod =="HIT08_54") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Other"),];
dataProf <- dataProf [(dataProf$Answer.option=="NO"),];
stats8<- computeStats(dataProf,dataProf$Answer.difficulty,"Only HIT08_54 - difficulty");

p2<- plotSingleLine(stats2,"HIT02_24");
p5<- plotSingleLine(stats5,"HIT05_35");
p7<- plotSingleLine(stats7,"HIT07_33");
p1<- plotSingleLine(stats1,"HIT01_8");
p3<- plotSingleLine(stats3,"HIT03_6");
p6<- plotSingleLine(stats6,"HIT06_51");
p8<- plotSingleLine(stats8,"HIT08_54");
p4<- plotSingleLine(stats4,"HIT04_7");

multiplot(p2, p5, p7, p1,cols=4);

multiplot(p3, p6, p8, p4, cols=4);

######################################################################3