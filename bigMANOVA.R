# MANOVA for EVERYTHING
# EAP
# 4-11-20

#load and process data
chimpis = read.table(file = "MANOVA.csv", header = T, sep = ",")
str(chimpis)
names(chimpis)

#condense dependent variables
y <- cbind(chimpis$SocialPlay, chimpis$Groom, chimpis$OtherAff, chimpis$Aggression, chimpis$NonInt, chimpis$Abnormal, chimpis$Travel, chimpis$Inactive)#combines dependent variables

#run manova
manova(y ~ Time * Condition * Life, data=chimpis, na.action=na.omit)
M1 <- manova(y ~ Time * Condition * Life, data=chimpis, na.action=na.omit)
summary(M1, tol=0)
summary.aov(M1)

##POST HOC

#significant difference in Social Play by Time, anova to analyze
AOV1 = aov(chimpis$SocialPlay ~ chimpis$Time)
summary(AOV1)
TukeyHSD(AOV1)

#significant difference in Social Play by Life, anova to analyze
AO2 = aov(chimpis$SocialPlay ~ chimpis$Life)
summary(AOV2)
TukeyHSD(AOV2)

#significant difference in Social Play by Time:Life, anova to analyze
#doesn't work
AOV3 = aov(chimpis$SocialPlay ~ chimpis$Time*Life)
summary(AOV3)
TukeyHSD(AOV3)
