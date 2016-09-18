
#INDEPENDENT ANALYSIS (WORKERS CONTRIBUTED TO SINGLE JAVA METHOD)
#CONFIDENCE LEVELS OF ALL ANSWERS segregated by Java method.
setwd("C://firefly//AnswerCorrelationAnalysis//")
sample <- read.csv("BugConfidenceLevels_Independent_Transposed.csv", header=T)


summary(sample)

#Testing assumptions

#Normality Test, all series failed the Shapiro normality test (so data is not normal)
shapiro.test(sample$HIT06_51_confidence) #p-value = 1.364e-08
shapiro.test(sample$HIT08_54_confidence) #p-value < 2.2e-16
shapiro.test(sample$HIT02_24_confidence) # p-value = 7.174e-05
shapiro.test(sample$HIT07_33_confidence) # p-value = 0.00119
shapiro.test(sample$HIT01_8_confidence) # p-value = 1.062e-12
shapiro.test(sample$HIT03_6_confidence) #   p-value = 5.407e-12
shapiro.test(sample$HIT04_7_confidence) # p-value < 2.2e-16
shapiro.test(sample$HIT05_35_confidence) #p-value = 0.0001301

#Variance homogeineity of variance (homoscedacity)
library(car)

leveneTest(sample$HIT06_51_confidence,sample$HIT05_35_confidence) #p-value  0.1682
leveneTest(sample$HIT06_51_confidence,sample$HIT08_54_confidence) #p-value 0.6274
leveneTest(sample$HIT06_51_confidence,sample$HIT02_24_confidence) #p-value 0.6957
leveneTest(sample$HIT06_51_confidence,sample$HIT07_33_confidence) #p-value 0.6877
leveneTest(sample$HIT06_51_confidence,sample$HIT01_8_confidence) #p-value  0.1767
leveneTest(sample$HIT06_51_confidence,sample$HIT03_6_confidence) #p-value 0.8905
leveneTest(sample$HIT06_51_confidence,sample$HIT04_7_confidence ) #p-value 0.4029

#ALL Passed the levene's test. (p-value>=0.05)

#ANOVA
oneway.test(sample$HIT06_51_confidence ~ sample$HIT05_35_confidence)
#F = 0.7647, num df = 4.000, denom df = 20.787, p-value = 0.5601 NOT SIGNIFICANT

#Kruskal-Wallis Test (non-parametric)
kruskal.test(sample$HIT06_51_confidence ~ sample$HIT05_35_confidence)
#Kruskal-Wallis chi-squared = 0.6998, df = 4, p-value = 0.9514 NOT SIGNIFICANT


#Pos hoc comparision
#Bonferroni of 8 groups = 8!/2*6! = 8*7/2 = 4*7 = 28
#significance level = 0.05 / 28 = 0.001785714

#Right skewed J8, J1 againts the Left skewed (J3, J4, J6)  SIGNIFICANT
wilcox.test(sample$HIT08_54_confidence,sample$HIT04_7_confidence) #W = 66391.5, p-value = 1.593e-12
wilcox.test(sample$HIT01_8_confidence,sample$HIT04_7_confidence) #W = 36256.5, p-value = 2.529e-12
wilcox.test(sample$HIT08_54_confidence,sample$HIT03_6_confidence) #W = 19559, p-value = 0.0001386
wilcox.test(sample$HIT01_8_confidence,sample$HIT03_6_confidence) #W = 10696, p-value = 8.726e-06
wilcox.test(sample$HIT01_8_confidence,sample$HIT06_51_confidence) #W = 6722.5, p-value = 3.218e-07
wilcox.test(sample$HIT08_54_confidence,sample$HIT06_51_confidence) #W = 12286.5, p-value = 7.801e-06

#Right skewed J8, J1 against neutral J2, J7, J5 SIGNIFICANT
wilcox.test(sample$HIT08_54_confidence,sample$HIT02_24_confidence) # W = 6397.5, p-value = 0.000621
wilcox.test(sample$HIT01_8_confidence,sample$HIT07_33_confidence) # W = 3310.5, p-value = 9.144e-05
wilcox.test(sample$HIT08_54_confidence,sample$HIT07_33_confidence) # W = 6061.5, p-value = 0.001194
wilcox.test(sample$HIT01_8_confidence,sample$HIT02_24_confidence) # W = 3515, p-value = 2.949e-05
wilcox.test(sample$HIT08_54_confidence,sample$HIT05_35_confidence) # W = 7000, p-value = 0.0003016
wilcox.test(sample$HIT01_8_confidence,sample$HIT05_35_confidence) # W = 3816, p-value = 2.1e-05


#Left skewed J4, J3, J6  against neutral J2, J7 SIGNIFICANT
wilcox.test(sample$HIT04_7_confidence,sample$HIT02_24_confidence) # W = 9940.5, p-value = 0.6366 NOT!
wilcox.test(sample$HIT04_7_confidence,sample$HIT07_33_confidence) # W = 9763.5, p-value = 0.8687 NOT!
wilcox.test(sample$HIT03_6_confidence,sample$HIT02_24_confidence) # W = 3478, p-value = 0.593 NOT!
wilcox.test(sample$HIT03_6_confidence,sample$HIT07_33_confidence) # W = 3344, p-value = 0.5546 NOT !
wilcox.test(sample$HIT06_51_confidence,sample$HIT02_24_confidence) # W = 1891, p-value = 0.828 NOT !
wilcox.test(sample$HIT06_51_confidence,sample$HIT07_33_confidence) # W = 1850.5, p-value = 0.9959 NOT !
wilcox.test(sample$HIT05_35_confidence, sample$HIT02_24_confidence) # W = 1055, p-value = 0.7161 NOT !
wilcox.test(sample$HIT05_35_confidence, sample$HIT07_33_confidence) # W = 1039, p-value = 0.9108 NOT !

#------------------------------------------------------------------------------------------------------

#ALL ANALYSIS (SAME WORKERS CONTRIBUTED TO DIFFERENT JAVA METHODS)
#Since workers participate in more than one Java method, samples are no independent.
setwd("C://firefly//AnswerCorrelationAnalysis//")
sample <- read.csv("BugConfidenceLevels_All_Transposed.csv", header=T)
#CONFIDENCE LEVELS OF ALL ANSWERS segregated by Java method.
 

summary(sample)

#Testing assumptions

#Normality Test, all series failed the Shapiro normality test (p-value <0.05)
shapiro.test(sample$HIT06_51_confidence) 
shapiro.test(sample$HIT08_54_confidence) 
shapiro.test(sample$HIT02_24_confidence) 
shapiro.test(sample$HIT07_33_confidence) 
shapiro.test(sample$HIT01_8_confidence) 
shapiro.test(sample$HIT03_6_confidence) 
shapiro.test(sample$HIT04_7_confidence) 
shapiro.test(sample$HIT05_35_confidence)

#Variance homogeineity of variance (homoscedacity)
library(car)

leveneTest(sample$HIT06_51_confidence,sample$HIT05_35_confidence) 
leveneTest(sample$HIT06_51_confidence,sample$HIT08_54_confidence) 
leveneTest(sample$HIT06_51_confidence,sample$HIT02_24_confidence) 
leveneTest(sample$HIT06_51_confidence,sample$HIT07_33_confidence) 
leveneTest(sample$HIT06_51_confidence,sample$HIT01_8_confidence) 
leveneTest(sample$HIT06_51_confidence,sample$HIT03_6_confidence) 
leveneTest(sample$HIT06_51_confidence,sample$HIT04_7_confidence ) 

#ALL Passed the levene's test (p-value>=0.05)

#ANOVA

oneway.test(sample$HIT06_51_confidence ~ sample$HIT05_35_confidence)
#F = 2.7722, num df = 5.000, denom df = 38.092, p-value = 0.0313 SIGNIFICANT!!!
#However I cannot trust, because the samples are not independent.

#Kruskal-Wallis Test (non-parametric)
kruskal.test(sample$HIT06_51_confidence ~ sample$HIT05_35_confidence)
#Kruskal-Wallis chi-squared = 11.6703, df = 5, p-value = 0.0396 SIGNIFICANT!!!


