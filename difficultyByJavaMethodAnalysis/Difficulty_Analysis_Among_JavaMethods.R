
#ALL ANALYSIS (WORKERS CONTRIBUTED TO DIFFERENT JAVA METHOD)
#Since workers participate in more than one Java method, samples are not independent.
setwd("C://firefly//SpectraAnalysis//")
sample <- read.csv("Difficulty_ByJavaMethod_data.csv", header=T)
summary(sample)

leveneTest(sample$J6_difficulty,sample$J5_difficulty) #FAILED test
leveneTest(sample$J6_difficulty,sample$J8_difficulty)  
leveneTest(sample$J6_difficulty,sample$J2_difficulty) #FAILED test
leveneTest(sample$J6_difficulty,sample$J7_difficulty) #FAILED test
leveneTest(sample$J6_difficulty,sample$J1_difficulty)  
leveneTest(sample$J6_difficulty,sample$J3_difficulty) #FAILED test
leveneTest(sample$J6_difficulty,sample$J4_difficulty )  

#4 failed the levene's test (p-value>=0.05)


#Pos hoc comparision
#Bonferroni of 8 groups = 8!/2*6! = 8*7/2 = 4*7 = 28
#significance level = 0.05 / 28 = 0.001785714

#Right skewed J8, J1 againts the Left skewed SIGNIFICANT
wilcox.test(sample$J8_difficulty,sample$J4_difficulty) #W = 6.5, p-value = 1.081e-10
wilcox.test(sample$J1_difficulty,sample$J4_difficulty) #W = 4.5, p-value = 2.82e-06
wilcox.test(sample$J8_difficulty,sample$J3_difficulty) #W = 8, p-value = 2.229e-07
wilcox.test(sample$J1_difficulty,sample$J3_difficulty) #W = 4.5, p-value = 5.629e-05
wilcox.test(sample$J1_difficulty,sample$J8_difficulty) #W = 150, p-value = 0.2638 NOT SIGNIFICANT

#Right skewed J8, J1 against neutral J2, J7 SIGNIFICANT
wilcox.test(sample$J8_difficulty,sample$J2_difficulty) #W = 24.5, p-value = 0.01456 NOT SIGNIFICANT
wilcox.test(sample$J1_difficulty,sample$J7_difficulty) #W = 12, p-value = 0.01374 NOT SIGNIFICANT
wilcox.test(sample$J8_difficulty,sample$J7_difficulty) #W = 21.5, p-value = 0.001256
wilcox.test(sample$J1_difficulty,sample$J2_difficulty) #W = 12, p-value = 0.05475 NOT SIGNIFICANT
wilcox.test(sample$J8_difficulty,sample$J5_difficulty) #W = 1, p-value = 1.632e-05 
wilcox.test(sample$J1_difficulty,sample$J5_difficulty) #W = 1.5, p-value = 0.0004287

#Left skewed J4, J3, J6, J5  against neutral J2, J7 SIGNIFICANT
wilcox.test(sample$J4_difficulty,sample$J2_difficulty) #W = 203, p-value = 0.001319 
wilcox.test(sample$J4_difficulty,sample$J7_difficulty) #W = 258.5, p-value = 0.001078 
wilcox.test(sample$J3_difficulty,sample$J2_difficulty) #W = 92, p-value = 0.004455 NOT SIGNIFICANT
wilcox.test(sample$J3_difficulty,sample$J7_difficulty) #W = 117, p-value = 0.004572 NOT SIGNIFICANT
wilcox.test(sample$J6_difficulty,sample$J2_difficulty) #W = 95.5, p-value = 0.006146 NOT SIGNIFICANT
wilcox.test(sample$J6_difficulty,sample$J7_difficulty) #W = 123.5, p-value = 0.004551 NOT SIGNIFICANT
wilcox.test(sample$J5_difficulty, sample$J2_difficulty) #W = 49, p-value = 0.01099 NOT SIGNIFICANT
wilcox.test(sample$J5_difficulty, sample$J7_difficulty) #W = 61.5, p-value = 0.01582 NOT SIGNIFICANT
wilcox.test(sample$J2_difficulty, sample$J7_difficulty) #W = 20, p-value = 0.6499 NOT SIGNIFICANT
wilcox.test(sample$J4_difficulty, sample$J3_difficulty) #W = 345, p-value = 0.5758 NOT SIGNIFICANT
wilcox.test(sample$J4_difficulty, sample$J6_difficulty) #W = 399.5, p-value = 0.2357 NOT SIGNIFICANT
wilcox.test(sample$J3_difficulty, sample$J6_difficulty) #W = 167, p-value = 0.6552 NOT SIGNIFICANT

#SUMMARY
# The only statistically significant differences are
#J1 versus J2, J3, J4, J5, 
#J8 versus J3, J4, J5, J7
#J4 versus J2, J7
#So out of the 28 pairs, 10 actually showed to be different in terms of difficulty. J1, J8, and J4

