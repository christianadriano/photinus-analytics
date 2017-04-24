#
# Compute correlation between difficulty and confidence by Java Method and by Profession
#

loadAnswers<- function(fileName){
  
  setwd("C://Users//chris//OneDrive//Documentos//GitHub//correlationGraphs//");
  
  data_all <- read.csv(fileName,header = TRUE,sep=",");
  
  dataf = data.frame(data_all);
  
}

#Analysis by profession
