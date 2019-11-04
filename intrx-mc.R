#2-Nov-2019
#EAP
#Interaction and matched control analysis

#load and process data
intrx.mc = read.table(file = "intrx-mc.csv", header = T, sep = ",")
str(intrx.mc)

#total interaction seconds
intrx.mc$total.intrx = intrx.mc$CAf + intrx.mc$HAf + intrx.mc$NAS + intrx.mc$Other + intrx.mc$Ab + intrx.mc$Tr + intrx.mc$In

#total matched control seconds
intrx.mc$total.mc = intrx.mc$CAfMC + intrx.mc$HAfMC + intrx.mc$NASMC + intrx.mc$OtherMC + intrx.mc$AbMC + intrx.mc$TrMC + intrx.mc$InMC

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

#graphs
hist(intrx.mc$Caf.d)
hist(intrx.mc$Haf.d)
hist(intrx.mc$NAS.d)
hist(intrx.mc$Other.d)
hist(intrx.mc$In.d)
hist(intrx.mc$Ab.d)
hist(intrx.mc$Tr.d)



#condense dependent variables
y <- cbind(intrx.mc$Caf.d, intrx.mc$Haf.d, intrx.mc$Nas.d, intrx.mc$Other.d, intrx.mc$Ab.d, intrx.mc$Tr.d, intrx.mc$In.d)#combines dependent variables

#run manova
manova(y ~ Condition * Life, data=intrx.mc, na.action=na.omit)
M1 <- manova(y ~ Condition * Life, data=intrx.mc, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)

#interaction is not significant, so it can be removed
manova(y ~ Condition + Life, data=intrx.mc, na.action=na.omit)
M1 <- manova(y ~ Condition + Life, data=intrx.mc, na.action=na.omit)
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


