#CB vs HB
#EAP
#3-4-20


#test of differences in behavior within the interaction session
#load and process data
intrx = read.table(file = "intrx.csv", header = T, sep = ",")
str(intrx)
names(intrx)

#total intrx seconds
intrx$total = intrx$SocialPlay + intrx$Groom + intrx$OtherAff + intrx$Aggression + intrx$NonInt + intrx$Abnormal + intrx$Travel + intrx$Inactive

#calculate proportions
intrx$SocialPlay.p = intrx$SocialPlay/intrx$total
intrx$Groom.p = intrx$Groom/intrx$total
intrx$OtherAff.p = intrx$OtherAff/intrx$total
intrx$Aggression.p = intrx$Aggression/intrx$total
intrx$NonInt.p = intrx$NonInt/intrx$total
intrx$Abnormal.p = intrx$Abnormal/intrx$total
intrx$Travel.p = intrx$Travel/intrx$total
intrx$Inactive.p = intrx$Inactive/intrx$total


aov(SocialPlay.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A1 <- aov(SocialPlay.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A1,tol=0)

aov(Groom.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A2 <- aov(Groom.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A2,tol=0)

aov(OtherAff.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A3 <- aov(OtherAff.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A3,tol=0)

aov(Aggression.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A4 <- aov(Aggression.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A4,tol=0)

aov(NonInt.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A5 <- aov(NonInt.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A5,tol=0)

aov(Abnormal.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A6 <- aov(Abnormal.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A6,tol=0)

aov(Travel.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A7 <- aov(Travel.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A7,tol=0)

aov(Inactive.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A8 <- aov(Inactive.p ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A8,tol=0)



#significant difference in social play by Life, anova to analyze
AOV1 = aov(intrx$SocialPlay.p ~ intrx$Life)
summary(AOV1)
TukeyHSD(AOV1)

#significant difference in aggression by Life, anova to analyze
AOV2 = aov(intrx$Aggression.p ~ intrx$Life)
summary(AOV2)
TukeyHSD(AOV2)
