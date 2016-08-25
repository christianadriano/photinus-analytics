#ARE LINES TO INSPECT CORRELATED TO ANSWERS/WORKERS?
# I COULD NOT FIND ANY STATISTICAL SIGNIFICANT DIFFERENCES

setwd("C://firefly//SubcrowdsCorrelation//")
sample <- read.csv("answerWorker_linesToInspect_data.csv",  header=T)

summary(sample)

shapiro.test(sample$answerPerWorker) #NOT NORMAL
shapiro.test(sample$linesToInspect) #NORMAL
shapiro.test(sample$answerPerWorker_outliers_cut) #NORMAL
shapiro.test(sample$linesToInspect_outliers_cut) #NORMAL
shapiro.test(sample$recall) #NORMAL
shapiro.test(sample$lines_to_inspect) #NOT NORMAL
shapiro.test(sample$workers) #NORMAL
shapiro.test(sample$lines_to_inspect_2) #NORMAL
shapiro.test(sample$answers) #NOT NORMAL
shapiro.test(sample$precision) #NORMAL

cor.test(sample$answerPerWorker, sample$linesToInspect, method="kendall") 
#z = 0.0695, p-value = 0.9446  NOT SIGNIFICANT

cor.test(sample$answerPerWorker_outliers_cut, sample$linesToInspect_outliers_cut, method="kendall") 
#z = 0.0912, p-value = 0.9274 NOT SIGNIFICANT

cor.test(sample$recall, sample$lines_to_inspect, method="kendall") 
#z = 2.8592, p-value = 0.004247 tau=0.7474764 SIGNIFICANT!!!! STRONG POSITIVE CORRELATION


cor.test(sample$recall, sample$lines_to_inspect_2, method="kendall") 
#z = 2.8592, p-value = 0.004247 tau=0.7474764 SIGNIFICANT!!!! STRONG POSITIVE CORRELATION

cor.test(sample$workers, sample$lines_to_inspect_2, method="pearson") 
#t = -2.7161, df = 10, p-value = 0.0217 tau=-0.6515677 SIGNIFICANT!!!! STRONG NEGATIVE CORRELATION


cor.test(sample$answers, sample$lines_to_inspect_2, method="kendall") 
#z = -1.0426, p-value = 0.2971 NOT SIGNIFICANT

cor.test(sample$precision, sample$lines_to_inspect_2, method="pearson") 
#t = 0.395, df = 10, p-value = 0.7012 NOT SIGNIFICANT

#--------------------------------------------------------------------------------------------

#REMOVED REDUNDANT ENTRIES: filter score >80%
#ARE LINES TO INSPECT CORRELATED TO ANSWERS/WORKERS?
# I COULD NOT FIND ANY STATISTICAL SIGNIFICANT DIFFERENCES

setwd("C://firefly//SubcrowdsCorrelation//")
sample <- read.csv("TopSubcrowdsPerFilter_data.csv",  header=T)

summary(sample)

shapiro.test(sample$precision) #NORMAL
shapiro.test(sample$lines_to_inspect) #NORMAL
shapiro.test(sample$workers) #NOT NORMAL
shapiro.test(sample$answers) #NOT NORMAL
shapiro.test(sample$recall) #NORMAL
shapiro.test(sample$answer_per_worker) #NOT NORMAL


cor.test(sample$recall, sample$lines_to_inspect, method="kendall")  #NOT SIGNIFICANT

cor.test(sample$answerPerWorker, sample$linesToInspect, method="kendall") 
#z = 0.0695, p-value = 0.9446  NOT SIGNIFICANT


#--------------------------------------------------------------------------------------------

#30 SUBCROWDS
#ARE LINES TO INSPECT CORRELATED TO ANSWERS/WORKERS?
# I COULD NOT FIND ANY STATISTICAL SIGNIFICANT DIFFERENCES

setwd("C://firefly//SubcrowdsCorrelation//")
sample <- read.csv("30_subcrowds_data.csv",  header=T)

summary(sample)

shapiro.test(sample$precision) #NOT NORMAL
shapiro.test(sample$lines_to_inspect) #NOT NORMAL
shapiro.test(sample$workers) #NOT NORMAL
shapiro.test(sample$answers) # NOT NORMAL
shapiro.test(sample$recall) #NORMAL
shapiro.test(sample$answer_per_worker) # NOT NORMAL

cor.test(sample$precision, sample$lines_to_inspect, method="kendall")  # SIGNIFICANT !!
#z = -2.3342, p-value = 0.01959 tau=-0.3099194 

cor.test(sample$recall, sample$lines_to_inspect, method="kendall")  #NOT SIGNIFICANT
#z = 0.8978, p-value = 0.3693

cor.test(sample$answer_per_worker, sample$lines_to_inspect, method="kendall") #NOT SIGNIFICANT
#z = -1.0037, p-value = 0.3155  

cor.test(sample$workers, sample$lines_to_inspect, method="kendall") #SIGNIFICANT
#z = -4.6617, p-value = 3.137e-06 tau=-0.614086  

cor.test(sample$answers, sample$lines_to_inspect, method="kendall") #SIGNIFICANT
#z = -4.481, p-value = 7.43e-06 tau=-0.5891084

cor.test(sample$precision, sample$answers, method="kendall")  # SIGNIFICANT
#z = 2.7704, p-value = 0.005599 tau=0.3604895 

cor.test(sample$precision, sample$workers, method="kendall")  # SIGNIFICANT
#z = 2.1633, p-value = 0.03051 tau=0.2820635  


cor.test(sample$recall, sample$answers, method="kendall")  # NOT SIGNIFICANT
#z = 0.7686, p-value = 0.4422

cor.test(sample$recall, sample$workers, method="kendall")  # NOT SIGNIFICANT
#z = 0.3755, p-value = 0.7073

#------------------------------------------------------------------------------
#LEARN HOW THIS WORKp

lm.out = lm(formula= sample$workers ~ sample$lines_to_inspect + I(sample$workers^2), data=sample)
summary(lm.out)

lm.out = lm(formula= sample$workers ~ sample$lines_to_inspect + I(sample$lines_to_inspect^2), data=sample)
summary(lm.out)

lm.out = lm(formula= sample$workers ~ sample$lines_to_inspect + I(1/log(workers,2)), data=sample)
summary(lm.out)
plot(lm.out)


