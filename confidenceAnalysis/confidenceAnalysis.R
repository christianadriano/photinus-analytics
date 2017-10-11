install.packages("gmodels")
library(gmodels)

setwd("C://firefly//AnswerAnalysis//")
sample <- read.csv("answerConfidence.csv",  header=T)
summary(sample)

hist(sample$confidence);
hist(sample$difficulty);

cor.test(sample$confidence, sample$difficulty, method="kendall")
result$statistic
result$p.value

#CHI Square
CrossTable(sample$confidence, sample$difficulty, fisher=FALSE, chisq=TRUE, 
           expected=TRUE, sresid=TRUE,format="SPSS")
