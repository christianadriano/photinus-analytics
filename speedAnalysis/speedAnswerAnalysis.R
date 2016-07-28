#-----------------------------------------------------------------------------
# SPEED Analysis
#plotting
install.packages('Rcpp')
install.packages('ggplot2')
library(ggplot2)
library(reshape2)

fileName ="speedAnalysis.csv"

setwd("C://firefly//SpeedAnalysis//")
sample <- read.csv(fileName,  header=T)

summary(sample)

xAxis <- as.numeric(sample$Hours.taken)

dat <- data.frame(
  Hours = as.numeric(xAxis, levels=xAxis),
  Precision = sample$Average.Precision_PV,
  Recall = sample$Average.Recall_PV
)

dat.m <- melt(dat, id.vars = "Hours")

ggplot(dat.m, aes(x=Hours, y=value, group=variable)) +
  xlab("Elapsed hours") +
  ylab("average precision or recall (%)") +
  ggtitle("Precision and Recall by time")+
  theme_bw()+
  theme(legend.justification=c(1,0), legend.position=c(1,0))+
  geom_point(aes(shape=variable), size=4.0)+
  geom_line(aes(linetype=variable), size=1.0)+
  scale_shape_manual(values=c(1,2))+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_x_continuous(breaks = round(seq(min(0), max(160), by = 7),1)) +
  scale_y_continuous(breaks = round(seq(min(30), max(100), by = 5),1))


