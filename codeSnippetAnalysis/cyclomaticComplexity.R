##
## Compute the cyclomatic complexity in Java source code
#input: a directory files for each code snippet
#output: csv file with each line containing the following:
#code snippet name, size in words, cyclomatic complexity 

#Formula based on: https://www.leepoint.net/principles_and_practices/complexity/complexity-java-method.html

#### MAIN ######################################################################

install.packages("stringr", repos='http://cran.us.r-project.org')
library("stringr")

#Load scripts
baseDir<- "C://Users//chris//OneDrive//Documentos//GitHub//";
dir<- paste(baseDir, "dataWrangling//dataframeUtil.R",sep="");
source(dir);
dirData <-paste(baseDir, "photinus-analytics//codeSnippetAnalysis//data//",sep="");

fileList <- list.files(dirData, full.names = TRUE);

dataframe <- readLines(fileList[1]);

print("Cyclomatic complexity");
for(fileName in fileList){
  cat(fileName,": ",computeFile(fileName));
}

##Process each file
computeFile <- function(fileName){
  dataframe <- readLines(fileName)
  ##remove empty lines
  dataframe <- removeEmptyElements(dataframe);
  
  ##TODO remove comments
  ##TODO remove tabs
  
  ##Count complexity factors
  factors <- c("if","else","case","default","return","for","while","do-while","break","continue","&&","\\|\\|", "\\?", ":","catch", "finally", "throw", "throws");
  complexity = 1;
  for(line in dataframe){
    line <- sapply(line, tolower);
    print(line);
    countVector <- str_count(line,factors);
    print(sum(countVector));
    complexity = complexity + sum(countVector);
  }
  return (complexity);
}

