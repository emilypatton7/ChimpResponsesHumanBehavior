#2-27-20
#EAP
#Latency

#install and load packages
install.packages("ggplot2")
install.package("dplyr")
library(ggplot2)
library(dplyr)


#load and process data
latency = read.table(file = "Latency.csv", header = T, sep = ",", na.strings = "")
str(latency)
names(latency)

#test differences in latency
aov(Latency ~ Condition * Life * Chimp, data=latency, na.action=na.omit)
A1 <- aov(Latency ~ Condition * Life * Chimp, data=latency, na.action=na.omit)
summary(A1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(A1)


#make graphics
#Summarize HSP by life history
x <- group_by(latency, Life) %>%  # Grouping function causes subsequent functions to aggregate latency by Condition
  summarize(life.mean = mean(Latency, na.rm = T), # na.rm = TRUE to remove missing values
            life.sd=sd(Latency, na.rm = T),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(Latency)), # of observations, excluding NAs. 
            life.se=life.sd/sqrt(n))

ggplot(data=x, aes(x=Life, y=life.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=life.mean, ymax=life.mean+life.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Early Life History") +
  ylab(expression(Latency)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#Social Play is significant, anova to analyze
AOV1 = aov(latency$Latency ~ latency$Life)
summary(AOV1)
TukeyHSD(AOV1)

