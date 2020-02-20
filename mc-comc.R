#2-Nov-2019
#EAP
#Matched control and Carry over matched control analysis

#load and process data
mc.comc = read.table(file = "MC-COMC.csv", header = T, sep = ",")
str(mc.comc)

#total matched control seconds
mc.comc$total.mc = mc.comc$CAfMC + mc.comc$HAfMC + mc.comc$NASMC + mc.comc$OtherMC + mc.comc$AbMC + mc.comc$TrMC + mc.comc$InMC

#total carry over matched control seconds
mc.comc$total.comc = mc.comc$CafCOMC + mc.comc$HafCOMC + mc.comc$NASCOMC + mc.comc$OtherCOMC + mc.comc$AbCOMC + mc.comc$TrCOMC + mc.comc$InCOMC

#calculate proportions matched control
mc.comc$CAfMC.p = mc.comc$CAfMC/mc.comc$total.mc
mc.comc$HAfMC.p = mc.comc$HAfMC/mc.comc$total.mc
mc.comc$NASMC.p = mc.comc$NASMC/mc.comc$total.mc
mc.comc$OtherMC.p = mc.comc$OtherMC/mc.comc$total.mc
mc.comc$AbMC.p = mc.comc$AbMC/mc.comc$total.mc
mc.comc$TrMC.p = mc.comc$TrMC/mc.comc$total.mc
mc.comc$InMC.p = mc.comc$InMC/mc.comc$total.mc

#calculate proportions carry over matched control
mc.comc$CafCOMC.p = mc.comc$CafCOMC/mc.comc$total.comc
mc.comc$HafCOMC.p = mc.comc$HafCOMC/mc.comc$total.comc
mc.comc$NASCOMC.p = mc.comc$NASCOMC/mc.comc$total.comc
mc.comc$OtherCOMC.p = mc.comc$OtherCOMC/mc.comc$total.comc
mc.comc$AbCOMC.p = mc.comc$AbCOMC/mc.comc$total.comc
mc.comc$TrCOMC.p = mc.comc$TrCOMC/mc.comc$total.comc
mc.comc$InCOMC.p = mc.comc$InCOMC/mc.comc$total.comc

#calculate difference of matched control and carry over matched control
mc.comc$Caf.d = mc.comc$CAfMC.p - mc.comc$CafCOMC.p
mc.comc$Haf.d = mc.comc$HAfMC.p - mc.comc$HafCOMC.p
mc.comc$NAS.d = mc.comc$NASMC.p - mc.comc$NASCOMC.p
mc.comc$Other.d = mc.comc$OtherMC.p - mc.comc$OtherCOMC.p
mc.comc$Ab.d = mc.comc$AbMC.p - mc.comc$AbCOMC.p
mc.comc$Tr.d = mc.comc$TrMC.p - mc.comc$TrCOMC.p
mc.comc$In.d = mc.comc$InMC.p - mc.comc$InCOMC.p

#graphs
hist(mc.comc$Caf.d)
hist(mc.comc$Haf.d)
hist(mc.comc$NASMC.d)
hist(mc.comc$Other.d)
hist(mc.comc$Ab.d)
hist(mc.comc$Tr.d)
hist(mc.comc$In.d)


#condense dependent variables
y <- cbind(mc.comc$Caf.d, mc.comc$Haf.d, mc.comc$NAS.d, mc.comc$Other.d, mc.comc$Ab.d, mc.comc$Tr.d, mc.comc$In.d)#combines dependent variables

#run manova
manova(y ~ Condition * Life + Chimp, data=mc.comc, na.action=na.omit)
M1 <- manova(y ~ Condition * Life + Chimp, data=mc.comc, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)


#response 6 = Travel is significant, paired t-test to analyze
t.test(co.comc$Tr.d ~ co.comc$Condition,  alternative = c("two.sided"), paired=F)
#chimpanzees travelled more in the carry over matched control after chimpanzee interaction


t.test(mc.comc$HAfMC.p, mc.comc$HafCOMC, paired=T) #p = .01726
t.test(mc.comc$CAfMC.p, mc.comc$CafCOMC.p, paired=T) #p = .0797
t.test(mc.comc$NASMC.p, mc.comc$NASCOMC.p, paired=T) #p = .5837
t.test(mc.comc$OtherMC.p, mc.comc$OtherCOMC.p, paired=T) #p = .4945
t.test(mc.comc$AbMC.p, mc.comc$AbCOMC.p, paired=T) #p = .901
t.test(mc.comc$TrMC.p, mc.comc$TrCOMC.p, paired=T) #p = .009589
t.test(mc.comc$InMC.p, mc.comc$InCOMC.p, paired=T) #p = .02706

#make graphics
x <- group_by(mc.comc, Condition) %>%  # Grouping function causes subsequent functions to aggregate intrx.mc by Condition
  summarize(cond.mean = mean(In.d, na.rm = TRUE), # na.rm = TRUE to remove missing values
            cond.sd=sd(In.d, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(In.d)), # of observations, excluding NAs. 
            cond.se=cond.sd/sqrt(n))

ggplot(data=x, aes(x=Condition, y=cond.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=cond.mean, ymax=cond.mean+cond.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Interaction Type") +
  ylab(expression(Time~Difference~(Matched~Control-Carry~Over~Matched~Control))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))


