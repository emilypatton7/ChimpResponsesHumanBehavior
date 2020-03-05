#4-March-20
#EAP
#Second Carry over and carry over matched control analysis

#load and process data
co.comc = read.table(file="3CO-COMC.csv", header = T, sep = ",")#load table into R
str(co.comc)#returns structure of table
names(co.comc)

#total carry over seconds
co.comc$total.co = co.comc$SocialPlayCO + co.comc$GroomCO + co.comc$OtherAffCO + co.comc$AggressionCO + co.comc$NonIntCO + co.comc$AbnormalCO + co.comc$TravelCO + co.comc$InactiveCO

#total carry over matched control seconds
co.comc$total.comc = co.comc$SocialPlayCOMC + co.comc$GroomCOMC + co.comc$OtherAffCOMC + co.comc$AggressionCOMC + co.comc$NonIntCOMC + co.comc$AbnormalCOMC+ co.comc$TravelCOMC + co.comc$InactiveCOMC

#calculate proportions CO
co.comc$SocialPlayCO.p = co.comc$SocialPlayCO/co.comc$total.co
co.comc$GroomCO.p = co.comc$GroomCO/co.comc$total.co
co.comc$OtherAffCO.p = co.comc$OtherAffCO/co.comc$total.co
co.comc$AggressionCO.p = co.comc$AggressionCO/co.comc$total.co
co.comc$NonIntCO.p = co.comc$NonIntCO/co.comc$total.co
co.comc$AbnormalCO.p = co.comc$AbnormalCO/co.comc$total.co
co.comc$TravelCO.p = co.comc$TravelCO/co.comc$total.co
co.comc$InactiveCO.p = co.comc$InactiveCO/co.comc$total.co

#calculate proportions COMC
co.comc$SocialPlayCOMC.p = co.comc$SocialPlayCOMC/co.comc$total.co
co.comc$GroomCOMC.p = co.comc$GroomCOMC/co.comc$total.co
co.comc$OtherAffCOMC.p = co.comc$OtherAffCOMC/co.comc$total.co
co.comc$AggressionCOMC.p = co.comc$AggressionCOMC/co.comc$total.comc
co.comc$NonIntCOMC.p = co.comc$NonIntCOMC/co.comc$total.comc
co.comc$AbnormalCOMC.p = co.comc$AbnormalCOMC/co.comc$total.comc
co.comc$TravelCOMC.p = co.comc$TravelCOMC/co.comc$total.comc
co.comc$InactiveCOMC.p = co.comc$InactiveCOMC/co.comc$total.comc

#calculate difference of CO and COMC
co.comc$SocialPlay.d = co.comc$SocialPlayCO.p - co.comc$SocialPlayCOMC.p
co.comc$Groom.d = co.comc$GroomCO.p - co.comc$GroomCOMC.p
co.comc$OtherAff.d = co.comc$OtherAffCO.p - co.comc$OtherAffCOMC.p
co.comc$Aggression.d = co.comc$AggressionCO.p - co.comc$AggressionCOMC.p
co.comc$NonInt.d = co.comc$NonIntCO.p - co.comc$NonIntCOMC.p
co.comc$Abnormal.d = co.comc$AbnormalCO.p - co.comc$AbnormalCOMC.p
co.comc$Travel.d = co.comc$TravelCO.p - co.comc$TravelCOMC.p
co.comc$Inactive.d = co.comc$InactiveCO.p - co.comc$InactiveCOMC.p


#repeated measures approach with MANOVA
#condense dependent variables
y <- cbind(co.comc$SocialPlay.d, co.comc$Groom.d, co.comc$OtherAff.d, co.comc$Aggression.d, co.comc$NonInt.d, co.comc$Abnormal.d, co.comc$Travel.d, co.comc$Inactive.d)#combines dependent variables


#run manova
manova(y ~ Condition * Life + Chimp, data=co.comc, na.action=na.omit)
M1 <- manova(y ~ Condition * Life + Chimp, data=co.comc, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)

#try a different way to analyze, paired t-test of interaction v matched control for each behavior, p = 0.007
t.test(co.comc$SocialPlayCO.p, co.comc$SocialPlayCOMC.p, paired=T) 
t.test(co.comc$GroomCO.p, co.comc$GroomCOMC.p, paired=T) 
t.test(co.comc$OtherAffCO.p, co.comc$OtherAffCOMC.p, paired=T)
t.test(co.comc$AggressionCO.p, co.comc$AggressionCOMC.p, paired=T) 
t.test(co.comc$NonIntCO.p, co.comc$NonIntCOMC.p, paired=T) 
t.test(co.comc$AbnormalCO.p, co.comc$AbnormalCOMC.p, paired=T) 
t.test(co.comc$TravelCO.p, co.comc$TravelCOMC.p, paired=T) 
t.test(co.comc$InactiveCO.p, co.comc$InactiveCOMC.p, paired=T) 
