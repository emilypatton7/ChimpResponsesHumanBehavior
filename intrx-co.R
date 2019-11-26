#2-Nov-19
#EAP
#Interaction and carry over analysis

#load and process data
intrx.co = read.table(file = "intrx-CO.csv", header = T, sep = ",")
str(intrx.co)

#total interaction seconds
intrx.co$total.intrx = intrx.co$CAF + intrx.co$HAF + intrx.co$NAS + intrx.co$Other + intrx.co$Ab + intrx.co$Tr + intrx.co$In + intrx.co$BO

#total carry over seconds
intrx.co$total.co = intrx.co$CAFCO + intrx.co$HAFCO + intrx.co$NASCO + intrx.co$OtherCO + intrx.co$AbCO + intrx.co$TrCO + intrx.co$InCO + intrx.co$BOCO

#calculate proportions intrx
intrx.co$CAF.p = intrx.co$CAF/intrx.co$total.intrx
intrx.co$HAF.p = intrx.co$HAF/intrx.co$total.intrx
intrx.co$NAS.p = intrx.co$NAS/intrx.co$total.intrx
intrx.co$Other.p = intrx.co$Other/intrx.co$total.intrx
intrx.co$Ab.p = intrx.co$Ab/intrx.co$total.intrx
intrx.co$In.p = intrx.co$In/intrx.co$total.intrx
intrx.co$Tr.p = intrx.co$Tr/intrx.co$total.intrx

#calculate proportions carry over
intrx.co$CAFCO.p = intrx.co$CAFCO/intrx.co$total.co
intrx.co$HAFCO.p = intrx.co$HAFCO/intrx.co$total.co
intrx.co$NASCO.p = intrx.co$NASCO/intrx.co$total.co
intrx.co$OtherCO.p = intrx.co$OtherCO/intrx.co$total.co
intrx.co$AbCO.p = intrx.co$AbCO/intrx.co$total.co
intrx.co$TrCO.p = intrx.co$TrCO/intrx.co$total.co
intrx.co$InCO.p = intrx.co$InCO/intrx.co$total.co

#calculate difference of interaction and carry over
intrx.co$Caf.d = intrx.co$CAF.p - intrx.co$CAFCO.p
intrx.co$Haf.d = intrx.co$HAF.p - intrx.co$HAFCO.p
intrx.co$Nas.d = intrx.co$NAS.p - intrx.co$NASCO.p
intrx.co$Other.d = intrx.co$Other.p - intrx.co$OtherCO.p
intrx.co$Ab.d = intrx.co$Ab.p - intrx.co$AbCO.p
intrx.co$Tr.d = intrx.co$Tr.p - intrx.co$TrCO.p
intrx.co$In.d = intrx.co$In.p - intrx.co$InCO.p

#graphs
hist(intrx.co$Caf.d)
hist(intrx.co$Haf.d)
hist(intrx.co$In.d)
hist(intrx.co$Nas.d)
hist(intrx.co$Other.d)
hist(intrx.co$Tr.d)
hist(intrx.co$Ab.d)



#condense dependent variables
y <- cbind(intrx.co$Caf.d, intrx.co$Haf.d, intrx.co$Nas.d, intrx.co$Other.d, intrx.co$Ab.d, intrx.co$Tr.d, intrx.co$In.d)#combines dependent variables

#run manova
manova(y ~ Condition * LIFE, data=intrx.co, na.action=na.omit)
M1 <- manova(y ~ Condition * LIFE, data=intrx.co, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)

#RE-RAN UP TO HERE

#interaction is not significant, so it can be removed
manova(y ~ Condition + LIFE, data=intrx.co, na.action=na.omit)
M1 <- manova(y ~ Condition + LIFE, data=intrx.co, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)

#can remove condition since it is not significant
manova(y ~ LIFE, data=intrx.co, na.action=na.omit)
M1 <- manova(y ~ LIFE, data=intrx.co, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)

#use graphics to analyze residuals
plot(residuals(M1))
qqnorm(residuals(M1))#this looks bad
qqline(residuals(M1))
hist(residuals(M1))#but this looks really good

#response 5 = Abnormal is significant, anova to analyze
AOV1 = aov(intrx.co$Ab.d ~ intrx.co$LIFE)
summary(AOV1)
TukeyHSD(AOV1)

#response 7 = Inactive is significant, anova to analyze
AOV1 = aov(intrx.co$Ab.d ~ intrx.co$LIFE)
summary(AOV1)
TukeyHSD(AOV1)

#for both variables, slight differences between "mom" and other life history categories

#try a different way to analyze
t.test(intrx.co$HAF.p, intrx.co$HAFCO.p, paired=T) #p = .000000000000005451
#paired t-test of interaction v matched control for each behavior, p = 0.007
t.test(intrx.co$CAF.p, intrx.co$CAFCO.p, paired=T)
t.test(intrx.co$NAS.p, intrx.co$NASCO.p, paired=T)
t.test(intrx.co$Other.p, intrx.co$OtherCO.p, paired=T) #p .00005129
t.test(intrx.co$Ab.p, intrx.co$AbCO.p, paired=T)
t.test(intrx.co$Tr.p, intrx.co$TrCO.p, paired=T)
t.test(intrx.co$In.p, intrx.co$InCO.p, paired=T) # p .00001018


