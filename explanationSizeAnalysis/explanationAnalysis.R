

#------------------------------------------------------------------------
#Analyzing the size of the explanations

setwd("C://firefly//AnswerAnalysis//")
sample <- read.csv("answerExplanation.csv",  header=T)
summary(sample)

##NOT NORMAL
shapiro.test(log(sample$explanationSize+1))
#data:  sample$explanationSize
#W = 0.6939, p-value < 2.2e-16

hist(sample$explanationSize)

boxplot(sample$YES_ExplanationSize,
        sample$NO_ExplanationSize,
        sample$IDK_ExplanationSize,
        ylab="Size (characters)", main="Explanation size by answer option", 
        names = c("YES", "NO", "IDK"))

wilcox.test(sample$YES_ExplanationSize,
            sample$NO_ExplanationSize)
