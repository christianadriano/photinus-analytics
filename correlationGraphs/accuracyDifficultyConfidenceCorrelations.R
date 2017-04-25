#
# Compute correlation among difficulty and confidence and accuracy
#

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
  dataf_confidence <- df [(lineNumbers) ,];
  TP <- sum(dataf_confidence$TP);
  TN <- sum(dataf_confidence$TN);
  FP <- sum(dataf_confidence$FP);
  FN <- sum(dataf_confidence$FN);
  outcome.df <- data.frame(TP,TN,FP,FN);
  return (outcome.df);
}

#Compute accuracy for each level of difficulty and confidence
computeAccuracy<- function(df){
  return((df$TP+df$TN)/(df$TP+df$TN+df$FP+df$FN));
}

#Compute precision for each level of difficulty and confidence
computePrecision<- function(df){
  result<-(df$TP)/(df$TP+df$FP);
  if(is.nan(sum(result))){
    index<- is.nan(result);
    result[index]<-0;
  }
  return(result);
}

#Compute precision for each level of difficulty and confidence
computeRecall<- function(df){
  return((df$TP)/(df$TP+df$FN));
}

## Computes a table with statistics for each difficulty or confidence level
computeStats<- function(df,selection, filterName){
 
  dataframe <- data.frame(Confidence=double(),Accuracy=double(), Precision=double(), Recall=double(), Answers=double() );
  
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


## ALL answers
stats1<- computeStats(dataf,dataf$Answer.confidence,"All Answers - confidence");

#### ANSWER OPTION
## Only YES's
dataYES <- dataf [(dataf$Answer.option=="YES") ,];
stats2<- computeStats(dataYES,dataYES$Answer.confidence,"Only YES Answers - confidence");

## Only NO's
dataNO <- dataf [(dataf$Answer.option=="NO") ,];
stats3<- computeStats(dataNO,dataNO$Answer.confidence,"Only NO Answers - confidence");

#Plotting Answer Options
install.packages("ggplot2");
library("ggplot2");
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//correlationGraphs//multiplot.R");

## Builds plots with multiple lines
plotMultiLine <- function(stats,title){

  multiplot <- ggplot(stats, aes(x=Confidence),fill="Metrics") +
  geom_line(aes(y = Accuracy, colour="Accuracy")) + 
  geom_line(aes(y = Recall, colour = "Recall")) +
  geom_line(aes(y = Precision, colour = "Precision")) +
  ggtitle(title) +
  ylab(label="Metrics") + 
  xlab("Confidence") +
  scale_colour_manual("", 
                      breaks = c("Accuracy" ,"Recall","Precision"),
                      values = c("Accuracy"="blue","Recall"="black","Precision"="red"))+
    scale_y_continuous(limits = c(0, 1));
 
   return(multiplot);
}

plotSingleLine <- function(stats,title){
  
  singlePlot <- ggplot(stats, aes(x=Confidence),fill="Metrics") +
    geom_line(aes(y = Accuracy, colour="Accuracy")) + 
    ggtitle(title) +
    ylab(label="Metrics") + 
    xlab("Confidence") +
     scale_colour_manual("", 
                         breaks = c("Accuracy"),
                         values = c("Accuracy"="blue"))+
  scale_y_continuous(limits = c(0, 1));
  return(singlePlot);
}

# breaks =c("Accuracy" ,"Recall","Precision")
p1<- plotMultiLine(stats1,"All answers");
p2<- plotSingleLine(stats2,"Only Yes answers");
p3<- plotSingleLine(stats3,"Only No answers");
multiplot(p1, p2, p3, cols=2);

##################################################################################






################################################################################

#### WORKER PROFESSION
## Only PROFESSIONAL_DEVELOPERS
dataProf <- dataf [(dataf$Worker.profession=="Professional_Developer") ,];
stats1<- computeStats(dataProf,dataProf$Answer.confidence,"Only PROFESSIONAL_DEVELOPERS - confidence");

## Only HOBBYIST´S
dataProf <- dataf [(dataf$Worker.profession=="Hobbyist") ,];
stats2<- computeStats(dataProf,dataProf$Answer.confidence,"Only HOBBYIST - confidence");

## Only GRADUATE_STUDENT´S
dataProf <- dataf [(dataf$Worker.profession=="Graduate_Student") ,];
stats3<- computeStats(dataProf,dataProf$Answer.confidence,"Only GRADUATE_STUDENT - confidence");

## Only UNDERGRADUATE_STUDENT´S
dataProf <- dataf [(dataf$Worker.profession=="Undergraduate_Student") ,];
stats4<- computeStats(dataProf,dataProf$Answer.confidence,"Only UNDERGRADUATE_STUDENT - confidence");

## Only OTHER´S
dataProf <- dataf [(dataf$Worker.profession=="Other") ,];
stats5<- computeStats(dataProf,dataProf$Answer.confidence,"Only OTHER - confidence");

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
stats1<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT01_8 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT02_24") ,];
stats2<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT02_24 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT03_6") ,];
stats3<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT03_6 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT04_7") ,];
stats4<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT04_7 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT05_35") ,];
stats5<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT05_35 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT06_51") ,];
stats6<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT06_51 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT07_33") ,];
stats7<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT07_33 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT08_54") ,];
stats8<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT08_54 - confidence");

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
# Java METHOD and PROFESSION

dataProf <- dataf [(dataf$FailingMethod =="HIT01_8") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats1<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT01_8 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT02_24") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats2<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT02_24 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT03_6") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats3<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT03_6 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT04_7") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats4<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT04_7 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT05_35") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats5<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT05_35 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT06_51") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats6<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT06_51 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT07_33") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats7<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT07_33 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT08_54") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Professional_Developer"),];
stats8<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT08_54 - confidence");

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
# Java METHOD and UNDERGRADS

dataProf <- dataf [(dataf$FailingMethod =="HIT01_8") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats1<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT01_8 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT02_24") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats2<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT02_24 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT03_6") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats3<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT03_6 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT04_7") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats4<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT04_7 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT05_35") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats5<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT05_35 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT06_51") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats6<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT06_51 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT07_33") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats7<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT07_33 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT08_54") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
stats8<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT08_54 - confidence");

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
# Java METHOD and UNDERGRADS and ANSWER OPTIONS

dataProf <- dataf [(dataf$FailingMethod =="HIT01_8") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats1<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT01_8 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT02_24") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats2<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT02_24 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT03_6") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats3<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT03_6 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT04_7") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats4<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT04_7 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT05_35") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats5<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT05_35 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT06_51") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats6<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT06_51 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT07_33") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats7<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT07_33 - confidence");

dataProf <- dataf [(dataf$FailingMethod =="HIT08_54") ,];
dataProf <- dataProf [(dataProf$Worker.profession=="Undergraduate_Student"),];
dataProf <- dataProf [(dataProf$Answer.option=="YES"),];
stats8<- computeStats(dataProf,dataProf$Answer.confidence,"Only HIT08_54 - confidence");

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
