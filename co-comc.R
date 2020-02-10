#1-Nov-19
#CPA and EAP
#Carry over and carry over matched control analysis

#load and process data
co.comc = read.table(file="CO-COMC.csv", header = T, sep = ",")#load table into R
str(co.comc)#returns structure of table
names(co.comc)

#total carry over seconds
co.comc$total.co = co.comc$CAfCO + co.comc$HAfCO + co.comc$NASCO + co.comc$OtherCO + co.comc$AbCO + co.comc$TrCO + co.comc$InCO + co.comc$BOCO

#total carry over matched control seconds
co.comc$total.comc = co.comc$CafCOMC + co.comc$HafCOMC + co.comc$NASCOMC + co.comc$OtherCOMC + co.comc$AbCOMC+ co.comc$TrCOMC + co.comc$InCOMC + co.comc$BOCOMC

#calculate proportions CO
co.comc$CAfCO.p = co.comc$CAfCO/co.comc$total.co
co.comc$HAfCO.p = co.comc$HAfCO/co.comc$total.co
co.comc$NASCO.p = co.comc$NASCO/co.comc$total.co
co.comc$OtherCO.p = co.comc$OtherCO/co.comc$total.co
co.comc$AbCO.p = co.comc$AbCO/co.comc$total.co
co.comc$TrCO.p = co.comc$TrCO/co.comc$total.co
co.comc$InCO.p = co.comc$InCO/co.comc$total.co

#calculate proportions COMC
co.comc$CAfCOMC.p = co.comc$CafCOMC/co.comc$total.comc
co.comc$HAfCOMC.p = co.comc$HafCOMC/co.comc$total.comc
co.comc$NASCOMC.p = co.comc$NASCOMC/co.comc$total.comc
co.comc$OtherCOMC.p = co.comc$OtherCOMC/co.comc$total.comc
co.comc$AbCOMC.p = co.comc$AbCOMC/co.comc$total.comc
co.comc$TrCOMC.p = co.comc$TrCOMC/co.comc$total.comc
co.comc$InCOMC.p = co.comc$InCOMC/co.comc$total.comc

#calculate difference of CO and COMC
co.comc$Caf.d = co.comc$CAfCO.p - co.comc$CAfCOMC.p
co.comc$Haf.d = co.comc$HAfCO.p - co.comc$HAfCOMC.p
co.comc$Nas.d = co.comc$NASCO.p - co.comc$NASCOMC.p
co.comc$Other.d = co.comc$OtherCO.p - co.comc$OtherCOMC.p
co.comc$Ab.d = co.comc$AbCO.p - co.comc$AbCOMC.p
co.comc$Tr.d = co.comc$TrCO.p - co.comc$TrCOMC.p
co.comc$In.d = co.comc$InCO.p - co.comc$InCOMC.p

#graphs
hist(co.comc$Caf.d)
hist(co.comc$Haf.d)
hist(co.comc$Nas.d)
hist(co.comc$Other.d)
hist(co.comc$Ab.d)
hist(co.comc$Tr.d)
hist(co.comc$In.d)


#repeated measures approach with MANOVA
#condense dependent variables
y <- cbind(co.comc$Caf.d, co.comc$Haf.d, co.comc$Nas.d, co.comc$Other.d, co.comc$Ab.d, co.comc$Tr.d, co.comc$In.d)#combines dependent variables


#run manova
manova(y ~ Condition * Life + Chimp, data=co.comc, na.action=na.omit)
M1 <- manova(y ~ Condition * Life + Chimp, data=co.comc, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)

#make graphics
x <- group_by(co.comc, Condition) %>%  # Grouping function causes subsequent functions to aggregate intrx.mc by Condition
  summarize(cond.mean = mean(In.d, na.rm = TRUE), # na.rm = TRUE to remove missing values
            cond.sd=sd(In.d, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(In.d)), # of observations, excluding NAs. 
            cond.se=cond.sd/sqrt(n))

ggplot(data=x, aes(x=Condition, y=cond.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=cond.mean, ymax=cond.mean+cond.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Interaction Type") +
  ylab(expression(Time~Difference~(Carry~Over-Carry~Over~Matched~Control))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))


##########
#things below here we tried and they are not the best approach
##########
#condense dependent variables
y <- cbind(co.comc$Caf.d, co.comc$Haf.d, co.comc$Nas.d, co.comc$Other.d, co.comc$Ab.d, co.comc$Tr.d, co.comc$In.d)#combines dependent variables


#run manova
manova(y ~ Condition * Life + Chimp, data=co.comc, na.action=na.omit)
M1 <- manova(y ~ Condition * Life + Chimp, data=co.comc, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)

#RE-RAN EVERYTHING UP TO HERE

#interaction is not significant, so it can be removed
manova(y ~ Condition + Life, data=co.comc, na.action=na.omit)
M1 <- manova(y ~ Condition + Life, data=co.comc, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)

#can remove life history since it is not significant
manova(y ~ Condition, data=co.comc, na.action=na.omit)
M1 <- manova(y ~ Condition, data=co.comc, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)

#use graphics to analyze residuals
plot(residuals(M1))
qqnorm(residuals(M1))#this looks bad
qqline(residuals(M1))
hist(residuals(M1))#but this looks really good

#response 6 = Travel is significant, paired t-test to analyze
t.test(co.comc$Tr.d ~ co.comc$Condition,  alternative = c("two.sided"), paired=F)
#chimpanzees travelled more in the carry over matched control after chimpanzee interaction

#try a different way to analyze, paired t-test of interaction v matched control for each behavior, p = 0.007
t.test(co.comc$HAfCO.p, co.comc$HAfCOMC.p, paired=T) #p = .09107
t.test(co.comc$CAfCO.p, co.comc$CAfCOMC.p, paired=T) #p = .7665
t.test(co.comc$NASCO.p, co.comc$NASCOMC.p, paired=T) #p = .9914
t.test(co.comc$OtherCO.p, co.comc$OtherCOMC.p, paired=T) #p = .3034
t.test(co.comc$AbCO.p, co.comc$AbCOMC.p, paired=T) #p = .7718
t.test(co.comc$TrCO.p, co.comc$TrCOMC.p, paired=T) #p = .01156
t.test(co.comc$InCO.p, co.comc$InCOMC.p, paired=T) #p = .688


