#2-Nov-2019
#EAP
#Interaction and matched control analysis

#install and load packages
install.packages("ggplot2")
install.package("dplyr")
library(ggplot2)
library(dplyr)

#load and process data
intrx.mc = read.table(file = "intrx-mc.csv", header = T, sep = ",")
str(intrx.mc)
names(intrx.mc)

#total interaction seconds
intrx.mc$total.intrx = intrx.mc$CAf + intrx.mc$HAf + intrx.mc$NAS + intrx.mc$Other + intrx.mc$Ab + intrx.mc$Tr + intrx.mc$In + intrx.mc$BO

#total matched control seconds
intrx.mc$total.mc = intrx.mc$CAfMC + intrx.mc$HAfMC + intrx.mc$NASMC + intrx.mc$OtherMC + intrx.mc$AbMC + intrx.mc$TrMC + intrx.mc$InMC + intrx.mc$BOMC

#calculate proportions interaction
intrx.mc$CAf.p = intrx.mc$CAf/intrx.mc$total.intrx
intrx.mc$HAf.p = intrx.mc$HAf/intrx.mc$total.intrx
intrx.mc$NAS.p = intrx.mc$NAS/intrx.mc$total.intrx
intrx.mc$Other.p = intrx.mc$Other/intrx.mc$total.intrx
intrx.mc$Ab.p = intrx.mc$Ab/intrx.mc$total.intrx
intrx.mc$Tr.p = intrx.mc$Tr/intrx.mc$total.intrx
intrx.mc$In.p = intrx.mc$In/intrx.mc$total.intrx

#calculate proportions matched control
intrx.mc$CAfMC.p = intrx.mc$CAfMC/intrx.mc$total.mc
intrx.mc$HAfMC.p = intrx.mc$HAfMC/intrx.mc$total.mc
intrx.mc$NASMC.p = intrx.mc$NASMC/intrx.mc$total.mc
intrx.mc$OtherMC.p = intrx.mc$OtherMC/intrx.mc$total.mc
intrx.mc$AbMC.p = intrx.mc$AbMC/intrx.mc$total.mc
intrx.mc$TrMC.p = intrx.mc$TrMC/intrx.mc$total.mc
intrx.mc$InMC.p = intrx.mc$InMC/intrx.mc$total.mc

#calculate difference of interaction and matched control
intrx.mc$Caf.d = intrx.mc$CAf.p - intrx.mc$CAfMC.p
intrx.mc$Haf.d = intrx.mc$HAf.p - intrx.mc$HAfMC.p
intrx.mc$NAS.d = intrx.mc$NAS.p - intrx.mc$NASMC.p
intrx.mc$Other.d = intrx.mc$Other.p - intrx.mc$OtherMC.p
intrx.mc$Ab.d = intrx.mc$Ab.p - intrx.mc$AbMC.p
intrx.mc$Tr.d = intrx.mc$Tr.p - intrx.mc$TrMC.p
intrx.mc$In.d = intrx.mc$In.p - intrx.mc$InMC.p

#normalize difference of interaction and matched control (convert back to raw duration)
intrx.mc$Caf.d = intrx.mc$Caf.d * 600
intrx.mc$Haf.d = intrx.mc$Haf.d * 600
intrx.mc$NAS.d = intrx.mc$NAS.d * 600
intrx.mc$Other.d = intrx.mc$Other.d * 600
intrx.mc$Ab.d = intrx.mc$Ab.d * 600
intrx.mc$Tr.d = intrx.mc$Tr.d * 600
intrx.mc$In.d = intrx.mc$In.d * 600

#graphs
hist(intrx.mc$Caf.d)
hist(intrx.mc$Haf.d)
hist(intrx.mc$NAS.d)
hist(intrx.mc$Other.d)
hist(intrx.mc$In.d)
hist(intrx.mc$Ab.d)
hist(intrx.mc$Tr.d)

#condense dependent variables as proportional differences 
y <- cbind(intrx.mc$Caf.d, intrx.mc$Haf.d, intrx.mc$NAS.d, intrx.mc$Other.d, intrx.mc$Ab.d, intrx.mc$Tr.d, intrx.mc$In.d)#combines dependent variables


#run manova
manova(y ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
M1 <- manova(y ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)

#make graphics
x <- group_by(intrx.mc, Condition) %>%  # Grouping function causes subsequent functions to aggregate intrx.mc by Condition
  summarize(cond.mean = mean(In.d, na.rm = TRUE), # na.rm = TRUE to remove missing values
            cond.sd=sd(In.d, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(In.d)), # of observations, excluding NAs. 
            cond.se=cond.sd/sqrt(n))

ggplot(data=x, aes(x=Condition, y=cond.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=cond.mean, ymax=cond.mean+cond.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Interaction Type") +
  ylab(expression(Time~Difference~(Interaction-Matched~Control))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#use graphics to analyze residuals
plot(residuals(M1))
qqnorm(residuals(M1))#this looks bad
qqline(residuals(M1))
hist(residuals(M1))#but this looks really good

#run paired t-tests to find difference between interaction and matched control
t.test(intrx.mc$HAf.p, intrx.mc$HAfMC.p, paired=T) #p = 7.299e-15
#paired t-test of interaction v matched control for each behavior, p = 0.007
t.test(intrx.mc$CAf.p, intrx.mc$CAfMC.p, paired=T) #p = .2351
t.test(intrx.mc$NAS.p, intrx.mc$NASMC.p, paired=T) #p = .02203
t.test(intrx.mc$Other.p, intrx.mc$OtherMC.p, paired=T) #p = .01397
t.test(intrx.mc$Ab.p, intrx.mc$AbMC.p, paired=T) #p = .1654
t.test(intrx.mc$Tr.p, intrx.mc$TrMC.p, paired=T) #p = .3631
t.test(intrx.mc$In.p, intrx.mc$InMC.p, paired=T) #p = .4.487e-09

#test of differences in behavior within the interaction session
aov(HAf ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A1 <- aov(HAf ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(A1)

aov(CAf ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A2 <- aov(CAf ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A2,tol=0)

aov(NAS ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A3 <- aov(NAS ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A3,tol=0)

aov(NAS ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A3 <- aov(NAS ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A3,tol=0)

aov(Ab ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A4 <- aov(Ab ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A4,tol=0)

aov(Tr ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A5 <- aov(Tr ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A5,tol=0)

aov(In ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A6 <- aov(In ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A6,tol=0)
