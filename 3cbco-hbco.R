#CBCO vs HBCO
#EAP
#3-4-20


#test of differences in behavior within the CO session
#load and process data
carryover = read.table(file = "3intrx-CO.csv", header = T, sep = ",")
str(carryover)
names(carryover)

#total carryover seconds
carryover$total = carryover$SocialPlayCO + carryover$GroomCO + carryover$OtherAffCO + carryover$AggressionCO + carryover$NonIntCO + carryover$AbnormalCO + carryover$TravelCO + carryover$InactiveCO

#calculate proportions
carryover$SocialPlayCO.p = carryover$SocialPlayCO/carryover$total
carryover$GroomCO.p = carryover$GroomCO/carryover$total
carryover$OtherAffCO.p = carryover$OtherAffCO/carryover$total
carryover$AggressionCO.p = carryover$AggressionCO/carryover$total
carryover$NonIntCO.p = carryover$NonIntCO/carryover$total
carryover$AbnormalCO.p = carryover$AbnormalCO/carryover$total
carryover$TravelCO.p = carryover$TravelCO/carryover$total
carryover$InactiveCO.p = carryover$InactiveCO/carryover$total


aov(SocialPlayCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A1 <- aov(SocialPlayCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A1,tol=0)

aov(GroomCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A2 <- aov(GroomCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A2,tol=0)

aov(OtherAffCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A3 <- aov(OtherAffCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A3,tol=0)

aov(AggressionCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A4 <- aov(AggressionCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A4,tol=0)

aov(NonIntCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A5 <- aov(NonIntCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A5,tol=0)

aov(AbnormalCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A6 <- aov(AbnormalCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A6,tol=0)

aov(TravelCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A7 <- aov(TravelCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A7,tol=0)

aov(InactiveCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A8 <- aov(InactiveCO.p ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A8,tol=0)


#significant difference in aggression by Life, anova to analyze
AOV1 = aov(carryover$AggressionCO.p ~ carryover$Life)
summary(AOV1)
TukeyHSD(AOV1)

#significant difference in abnormal by Life, anova to analyze
AOV2 = aov(carryover$AbnormalCO.p ~ carryover$Life)
summary(AOV2)
TukeyHSD(AOV2)

