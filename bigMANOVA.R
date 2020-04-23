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
AOV2 = aov(chimpis$SocialPlay ~ chimpis$Life)
summary(AOV2)
TukeyHSD(AOV2)

#significant difference in Social Play by Time:Life, anova to analyze
AOV3 = aov(chimpis$SocialPlay ~ chimpis$Time:chimpis$Life)
summary(AOV3)
TukeyHSD(AOV3)


#significant difference in Groom by Time, anova to analyze
AOV4 = aov(chimpis$Groom ~ chimpis$Time)
summary(AOV4)
TukeyHSD(AOV4)

#significant difference in Other Affinitive by Time, anova to analyze
AOV5 = aov(chimpis$OtherAff ~ chimpis$Time)
summary(AOV5)
TukeyHSD(AOV5)

#significant difference in Other Affinitive by Time:Condition, anova to analyze
AOV6 = aov(chimpis$OtherAff ~ chimpis$Time:chimpis$Condition)
summary(AOV6)
TukeyHSD(AOV6)


#significant difference in Aggression by Time, anova to analyze
AOV7 = aov(chimpis$Aggression ~ chimpis$Time)
summary(AOV7)
TukeyHSD(AOV7)

#significant difference in Aggression by Life, anova to analyze
AOV8 = aov(chimpis$Aggression ~ chimpis$Life)
summary(AOV8)
TukeyHSD(AOV8)

#significant difference in Aggression by Time:Life, anova to analyze
AOV9 = aov(chimpis$Aggression ~ chimpis$Time:chimpis$Life)
summary(AOV9)
TukeyHSD(AOV9)

#significant difference in Non-Interactive by Time, anova to analyze
AOV10 = aov(chimpis$NonInt ~ chimpis$Time)
summary(AOV10)
TukeyHSD(AOV10)

#significant difference in Non-Interactive by Condition, anova to analyze
AOV11 = aov(chimpis$NonInt ~ chimpis$Condition)
summary(AOV11)
TukeyHSD(AOV11)

#significant difference in Non-Interactive by Life, anova to analyze
AOV12 = aov(chimpis$NonInt ~ chimpis$Life)
summary(AOV12)
TukeyHSD(AOV12)

#significant difference in Abnormal by Condition, anova to analyze
AOV13 = aov(chimpis$Abnormal ~ chimpis$Condition)
summary(AOV13)
TukeyHSD(AOV13)

#significant difference in Abnormal by Life, anova to analyze
AOV14 = aov(chimpis$Abnormal ~ chimpis$Life)
summary(AOV14)
TukeyHSD(AOV14)

#significant difference in Travel by Time, anova to analyze
AOV15 = aov(chimpis$Travel ~ chimpis$Time)
summary(AOV15)
TukeyHSD(AOV15)

#significant difference in Travel by Life, anova to analyze
AOV16 = aov(chimpis$Travel ~ chimpis$Life)
summary(AOV16)
TukeyHSD(AOV16)

#significant difference in Travel by Time:Condition, anova to analyze
AOV17 = aov(chimpis$Travel ~ chimpis$Time:chimpis$Condition)
summary(AOV17)
TukeyHSD(AOV17)

#significant difference in Inactive by Time, anova to analyze
AOV18 = aov(chimpis$Inactive ~ chimpis$Time)
summary(AOV18)
TukeyHSD(AOV18)

#significant difference in Inactive by Condition, anova to analyze
AOV19 = aov(chimpis$Inactive ~ chimpis$Condition)
summary(AOV19)
TukeyHSD(AOV19)

#significant difference in Groom by Time:Condition, anova to analyze
AOV20 = aov(chimpis$Groom ~ chimpis$Time:chimpis$Condition)
summary(AOV20)
TukeyHSD(AOV20)




##try some contrasts
#didn't work, I don't understand the matrices

IT_vs_CO <- c(0, 0, 1, 0)
IT_vs_MC <- c(0, 0, 0, 1)
CO_vs_COMC <- c(0, 1, 0, 0)
contrasts(chimpis$Time) <- cbind(IT_vs_CO, IT_vs_MC, CO_vs_COMC)

SocialPlayModel <- lm(SocialPlay ~ Time, data=chimpis)
summary.lm(SocialPlayModel)

