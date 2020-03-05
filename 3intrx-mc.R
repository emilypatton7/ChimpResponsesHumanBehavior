#4-March-2020
#EAP
#Inactiveteraction and matched control analysis

#load and process data
intrx.mc = read.table(file = "3intrx-mc.csv", header = T, sep = ",")
str(intrx.mc)
names(intrx.mc)

#total interaction seconds
intrx.mc$total.intrx = intrx.mc$SocialPlay + intrx.mc$Groom + intrx.mc$OtherAff + intrx.mc$Aggression + intrx.mc$NonInt + intrx.mc$Abnormal + intrx.mc$Travel + intrx.mc$Inactive

#total matched control seconds
intrx.mc$total.mc = intrx.mc$SocialPlayMC + intrx.mc$GroomMC + intrx.mc$OtherAffMC + intrx.mc$AggressionMC + intrx.mc$NonIntMC + intrx.mc$AbnormalMC + intrx.mc$TravelMC + intrx.mc$InactiveMC

#calculate proportions interaction
intrx.mc$SocialPlay.p = intrx.mc$SocialPlay/intrx.mc$total.intrx
intrx.mc$Groom.p = intrx.mc$Groom/intrx.mc$total.intrx
intrx.mc$OtherAff.p = intrx.mc$OtherAff/intrx.mc$total.intrx
intrx.mc$Aggression.p = intrx.mc$Aggression/intrx.mc$total.intrx
intrx.mc$NonInt.p = intrx.mc$NonInt/intrx.mc$total.intrx
intrx.mc$Abnormal.p = intrx.mc$Abnormal/intrx.mc$total.intrx
intrx.mc$Travel.p = intrx.mc$Travel/intrx.mc$total.intrx
intrx.mc$Inactive.p = intrx.mc$Inactive/intrx.mc$total.intrx

#calculate proportions matched control
intrx.mc$SocialPlayMC.p = intrx.mc$SocialPlayMC/intrx.mc$total.mc
intrx.mc$GroomMC.p = intrx.mc$GroomMC/intrx.mc$total.mc
intrx.mc$OtherAffMC.p = intrx.mc$OtherAffMC/intrx.mc$total.mc
intrx.mc$AggressionMC.p = intrx.mc$AggressionMC/intrx.mc$total.mc
intrx.mc$NonIntMC.p = intrx.mc$NonIntMC/intrx.mc$total.mc
intrx.mc$AbnormalMC.p = intrx.mc$AbnormalMC/intrx.mc$total.mc
intrx.mc$TravelMC.p = intrx.mc$TravelMC/intrx.mc$total.mc
intrx.mc$InactiveMC.p = intrx.mc$InactiveMC/intrx.mc$total.mc

#calculate difference of interaction and matched control
intrx.mc$SocialPlay.d = intrx.mc$SocialPlay.p - intrx.mc$SocialPlayMC.p
intrx.mc$Groom.d = intrx.mc$Groom.p - intrx.mc$GroomMC.p
intrx.mc$OtherAff.d = intrx.mc$OtherAff.p - intrx.mc$OtherAffMC.p
intrx.mc$Aggression.d = intrx.mc$Aggression.p - intrx.mc$AggressionMC.p
intrx.mc$NonInt.d = intrx.mc$NonInt.p - intrx.mc$NonIntMC.p
intrx.mc$Abnormal.d = intrx.mc$Abnormal.p - intrx.mc$AbnormalMC.p
intrx.mc$Travel.d = intrx.mc$Travel.p - intrx.mc$TravelMC.p
intrx.mc$Inactive.d = intrx.mc$Inactive.p - intrx.mc$InactiveMC.p



#condense dependent variables as proportional differences 
y <- cbind(intrx.mc$SocialPlay.d, intrx.mc$Groom.d, intrx.mc$OtherAff.d, intrx.mc$Aggression.d, intrx.mc$NonInt.d, intrx.mc$Abnormal.d, intrx.mc$Travel.d, intrx.mc$Inactive.d)#combines dependent variables


#run manova
manova(y ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
M1 <- manova(y ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)


#run paired t-tests to find difference between interaction and matched control
#paired t-test of interaction v matched control for each behavior, p = 0.007t.test(intrx.mc$SocialPlay.p, intrx.mc$SocialPlayMC.p, paired=T) 
t.test(intrx.mc$SocialPlay.p, intrx.mc$SocialPlay.p, paired = T)
t.test(intrx.mc$Groom.p, intrx.mc$GroomMC.p, paired=T) 
t.test(intrx.mc$OtherAff.p, intrx.mc$OtherAffMC.p, paired=T) 
t.test(intrx.mc$Aggression.p, intrx.mc$AggressionMC.p, paired=T) 
t.test(intrx.mc$NonInt.p, intrx.mc$NonIntMC.p, paired=T) 
t.test(intrx.mc$Abnormal.p, intrx.mc$AbnormalMC.p, paired=T) 
t.test(intrx.mc$Travel.p, intrx.mc$TravelMC.p, paired=T) 
t.test(intrx.mc$Inactive.p, intrx.mc$InactiveMC.p, paired=T) 

#make graphics
#Summarize SocialPlay by life history
x <- group_by(intrx.mc, Life) %>%  # Grouping function causes subsequent functions to aggregate intrx.mc by Condition
  summarize(life.mean = mean(SocialPlay.d, na.rm = TRUE), # na.rm = TRUE to remove missing values
            life.sd=sd(SocialPlay.d, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(SocialPlay.d)), # of observations, excluding NAs. 
            life.se=life.sd/sqrt(n))

ggplot(data=x, aes(x=Life, y=life.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=life.mean, ymax=life.mean+life.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Inactiveteraction Type") +
  ylab(expression(Time~Difference~(Inactiveteraction-Matched~Control))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#Summarize Other by Chimp
x <- group_by(intrx.mc, Chimp) %>%  # Grouping function causes subsequent functions to aggregate intrx.mc by Condition
  summarize(Chimp.mean = mean(Other.d, na.rm = TRUE), # na.rm = TRUE to remove missing values
            Chimp.sd=sd(Other.d, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(Other.d)), # of observations, excluding NAs. 
            Chimp.se=Chimp.sd/sqrt(n))

ggplot(data=x, aes(x=Chimp, y=Chimp.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=Chimp.mean, ymax=Chimp.mean+Chimp.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Inactiveteraction Type") +
  ylab(expression(Time~Difference~(Inactiveteraction-Matched~Control))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#Summarize Aggression by Chimp
x <- group_by(intrx.mc, Chimp) %>%  # Grouping function causes subsequent functions to aggregate intrx.mc by Condition
  summarize(Chimp.mean = mean(Aggression.d, na.rm = TRUE), # na.rm = TRUE to remove missing values
            Chimp.sd=sd(Aggression.d, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(Aggression.d)), # of observations, excluding NAs. 
            Chimp.se=Chimp.sd/sqrt(n))

ggplot(data=x, aes(x=Chimp, y=Chimp.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=Chimp.mean, ymax=Chimp.mean+Chimp.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Inactiveteraction Type") +
  ylab(expression(Time~Difference~(Inactiveteraction-Matched~Control))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

