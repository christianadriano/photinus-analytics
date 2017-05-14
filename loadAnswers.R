#Load Answers into a dataframe
#It also removes invalid input and outliers

loadAnswers<- function(){
  
  setwd("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//");
  fileName = "answerList_data.csv";
  
  data_all <- read.csv(fileName,header = TRUE,sep=",");
  
  #First I need to look at the outliers or invalid values
  #Invalid age and invalid years of experience
  
  #Remove rows with not available data
  dataf <- data_all [rowSums(is.na(data_all))==0,]
  
  summary(dataf);
  
  #Remove Invalid age and invalid years of experience
  dataf <- dataf [!dataf$Worker.yearsOfExperience <0,]
  dataf <- dataf [!dataf$Worker.age <18,] ##minimum age to participate
  
  #Remove people with no experience in programming
#  dataf <- dataf [!dataf$Worker.yearsOfExperience ==0,]
  
  #Remove people whose age doesn´t macht with experience 
  dataf <- dataf [!(dataf$Worker.age - dataf$Worker.yearsOfExperience) <5,]
  
  #Remove outliers
  dataf <- dataf [!dataf$Worker.yearsOfExperience >50,]
  dataf <- dataf [!dataf$Worker.age >80,]
  
  #summary (dataf);
  cat("Data successfully loaded. Number of entries:",length(dataf[,1]),"\n");
  return(dataf);
}
