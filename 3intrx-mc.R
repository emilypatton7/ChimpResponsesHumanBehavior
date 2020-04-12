#4-March-2020
#EAP
#Inactiveteraction and matched control analysis

#load and process data
intrx.mc = read.table(file = "3intrx-mc.csv", header = T, sep = ",")
str(intrx.mc)
names(intrx.mc)

#total intrx seconds
intrx.mc$total.intrx = intrx.mc$SocialPlay + intrx.mc$Groom + intrx.mc$OtherAff + intrx.mc$Aggression + intrx.mc$NonInt + intrx.mc$Abnormal + intrx.mc$Travel + intrx.mc$Inactive

#total MC seconds
intrx.mc$total.mc = intrx.mc$SocialPlayMC + intrx.mc$GroomMC + intrx.mc$OtherAffMC + intrx.mc$AggressionMC + intrx.mc$NonIntMC + intrx.mc$AbnormalMC + intrx.mc$TravelMC + intrx.mc$InactiveMC

#calculate proportions intrx
intrx.mc$SocialPlay.p = intrx.mc$SocialPlay/intrx.mc$total.intrx
intrx.mc$Groom.p = intrx.mc$Groom/intrx.mc$total.intrx
intrx.mc$OtherAff.p = intrx.mc$OtherAff/intrx.mc$total.intrx
intrx.mc$Aggression.p = intrx.mc$Aggression/intrx.mc$total.intrx
intrx.mc$NonInt.p = intrx.mc$NonInt/intrx.mc$total.intrx
intrx.mc$Abnormal.p = intrx.mc$Abnormal/intrx.mc$total.intrx
intrx.mc$Travel.p = intrx.mc$Travel/intrx.mc$total.intrx
intrx.mc$Inactive.p = intrx.mc$Inactive/intrx.mc$total.intrx



#calculate proportions mc
intrx.mc$SocialPlayMC.p = intrx.mc$SocialPlayMC/intrx.mc$total.intrx
intrx.mc$GroomMC.p = intrx.mc$GroomMC/intrx.mc$total.intrx
intrx.mc$OtherAffMC.p = intrx.mc$OtherAffMC/intrx.mc$total.intrx
intrx.mc$AggressionMC.p = intrx.mc$AggressionMC/intrx.mc$total.intrx
intrx.mc$NonIntMC.p = intrx.mc$NonIntMC/intrx.mc$total.intrx
intrx.mc$AbnormalMC.p = intrx.mc$AbnormalMC/intrx.mc$total.intrx
intrx.mc$TravelMC.p = intrx.mc$TravelMC/intrx.mc$total.intrx
intrx.mc$InactiveMC.p = intrx.mc$InactiveMC/intrx.mc$total.intrx


#calculate difference of  and MC
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
t.test(intrx.mc$SocialPlay.p, intrx.mc$SocialPlayMC.p, paired = T)
t.test(intrx.mc$Groom.p, intrx.mc$GroomMC.p, paired=T) 
t.test(intrx.mc$OtherAff.p, intrx.mc$OtherAffMC.p, paired=T) 
t.test(intrx.mc$Aggression.p, intrx.mc$AggressionMC.p, paired=T) 
t.test(intrx.mc$NonInt.p, intrx.mc$NonIntMC.p, paired=T) 
t.test(intrx.mc$Abnormal.p, intrx.mc$AbnormalMC.p, paired=T) 
t.test(intrx.mc$Travel.p, intrx.mc$TravelMC.p, paired=T) 
t.test(intrx.mc$Inactive.p, intrx.mc$InactiveMC.p, paired=T) 

###post-hoc tests for differences between intrx and MC based on life history?

#load and process data
intrx.mc.mom = read.table(file = "intrx-MCmom.csv", header = T, sep = ",")
str(intrx.mc.mom)
names(intrx.mc.mom)

#total intrx semcnds
intrx.mc.mom$total.intrx = intrx.mc.mom$SocialPlay + intrx.mc.mom$Groom + intrx.mc.mom$OtherAff + intrx.mc.mom$Aggression + intrx.mc.mom$NonInt + intrx.mc.mom$Abnormal + intrx.mc.mom$Travel + intrx.mc.mom$Inactive

#total MC semcnds
intrx.mc.mom$total.mc = intrx.mc.mom$SocialPlayMC + intrx.mc.mom$GroomMC + intrx.mc.mom$OtherAffMC + intrx.mc.mom$AggressionMC + intrx.mc.mom$NonIntMC + intrx.mc.mom$AbnormalMC + intrx.mc.mom$TravelMC + intrx.mc.mom$InactiveMC

#calculate proportions intrx
intrx.mc.mom$SocialPlay.p = intrx.mc.mom$SocialPlay/intrx.mc.mom$total.intrx
intrx.mc.mom$Aggression.p = intrx.mc.mom$Aggression/intrx.mc.mom$total.intrx

#calculate proportions mc
intrx.mc.mom$SocialPlayMC.p = intrx.mc.mom$SocialPlayMC/intrx.mc.mom$total.intrx
intrx.mc.mom$AggressionMC.p = intrx.mc.mom$AggressionMC/intrx.mc.mom$total.intrx

t.test(intrx.mc.mom$SocialPlay, intrx.mc.mom$SocialPlayMC, paired=T)
t.test(intrx.mc.mom$Aggression, intrx.mc.mom$AggressionMC, paired=T)


intrx.mc.early = read.table(file = "intrx-MCearly.csv", header = T, sep = ",")
str(intrx.mc.early)
names(intrx.mc.early)

#total intrx semcnds
intrx.mc.early$total.intrx = intrx.mc.early$SocialPlay + intrx.mc.early$Groom + intrx.mc.early$OtherAff + intrx.mc.early$Aggression + intrx.mc.early$NonInt + intrx.mc.early$Abnormal + intrx.mc.early$Travel + intrx.mc.early$Inactive

#total MC semcnds
intrx.mc.early$total.mc = intrx.mc.early$SocialPlayMC + intrx.mc.early$GroomMC + intrx.mc.early$OtherAffMC + intrx.mc.early$AggressionMC + intrx.mc.early$NonIntMC + intrx.mc.early$AbnormalMC + intrx.mc.early$TravelMC + intrx.mc.early$InactiveMC

#calculate proportions intrx
intrx.mc.early$SocialPlay.p = intrx.mc.early$SocialPlay/intrx.mc.early$total.intrx
intrx.mc.early$Aggression.p = intrx.mc.early$Aggression/intrx.mc.early$total.intrx

#calculate proportions mc
intrx.mc.early$SocialPlayMC.p = intrx.mc.early$SocialPlayMC/intrx.mc.early$total.intrx
intrx.mc.early$AggressionMC.p = intrx.mc.early$AggressionMC/intrx.mc.early$total.intrx


t.test(intrx.mc.early$SocialPlay, intrx.mc.early$SocialPlayMC, paired=T) 
t.test(intrx.mc.early$Aggression, intrx.mc.early$AggressionMC, paired=T)


intrx.mc.late = read.table(file = "intrx-MClate.csv", header = T, sep = ",")
str(intrx.mc.late)
names(intrx.mc.late)

#total intrx semcnds
intrx.mc.late$total.intrx = intrx.mc.late$SocialPlay + intrx.mc.late$Groom + intrx.mc.late$OtherAff + intrx.mc.late$Aggression + intrx.mc.late$NonInt + intrx.mc.late$Abnormal + intrx.mc.late$Travel + intrx.mc.late$Inactive

#total MC semcnds
intrx.mc.late$total.mc = intrx.mc.late$SocialPlayMC + intrx.mc.late$GroomMC + intrx.mc.late$OtherAffMC + intrx.mc.late$AggressionMC + intrx.mc.late$NonIntMC + intrx.mc.late$AbnormalMC + intrx.mc.late$TravelMC + intrx.mc.late$InactiveMC

#calculate proportions intrx
intrx.mc.late$SocialPlay.p = intrx.mc.late$SocialPlay/intrx.mc.late$total.intrx
intrx.mc.late$Aggression.p = intrx.mc.late$Aggression/intrx.mc.late$total.intrx

#calculate proportions mc
intrx.mc.late$SocialPlayMC.p = intrx.mc.late$SocialPlayMC/intrx.mc.late$total.intrx
intrx.mc.late$AggressionMC.p = intrx.mc.late$AggressionMC/intrx.mc.late$total.intrx


t.test(intrx.mc.late$SocialPlay, intrx.mc.late$SocialPlayMC, paired=T) 
t.test(intrx.mc.late$Aggression, intrx.mc.late$AggressionMC, paired=T)


intrx.mc.nurse = read.table(file = "intrx-MCnurse.csv", header = T, sep = ",")
str(intrx.mc.nurse)
names(intrx.mc.nurse)

#total intrx semcnds
intrx.mc.nurse$total.intrx = intrx.mc.nurse$SocialPlay + intrx.mc.nurse$Groom + intrx.mc.nurse$OtherAff + intrx.mc.nurse$Aggression + intrx.mc.nurse$NonInt + intrx.mc.nurse$Abnormal + intrx.mc.nurse$Travel + intrx.mc.nurse$Inactive

#total MC semcnds
intrx.mc.nurse$total.mc = intrx.mc.nurse$SocialPlayMC + intrx.mc.nurse$GroomMC + intrx.mc.nurse$OtherAffMC + intrx.mc.nurse$AggressionMC + intrx.mc.nurse$NonIntMC + intrx.mc.nurse$AbnormalMC + intrx.mc.nurse$TravelMC + intrx.mc.nurse$InactiveMC

#calculate proportions intrx
intrx.mc.nurse$SocialPlay.p = intrx.mc.nurse$SocialPlay/intrx.mc.nurse$total.intrx
intrx.mc.nurse$Aggression.p = intrx.mc.nurse$Aggression/intrx.mc.nurse$total.intrx

#calculate proportions mc
intrx.mc.nurse$SocialPlayMC.p = intrx.mc.nurse$SocialPlayMC/intrx.mc.nurse$total.intrx
intrx.mc.nurse$AggressionMC.p = intrx.mc.nurse$AggressionMC/intrx.mc.nurse$total.intrx


t.test(intrx.mc.nurse$SocialPlay, intrx.mc.nurse$SocialPlayMC, paired=T) 
t.test(intrx.mc.nurse$Aggression, intrx.mc.nurse$AggressionMC, paired=T)


intrx.mc.pet = read.table(file = "intrx-MCpet.csv", header = T, sep = ",")
str(intrx.mc.pet)
names(intrx.mc.pet)

#total intrx semcnds
intrx.mc.pet$total.intrx = intrx.mc.pet$SocialPlay + intrx.mc.pet$Groom + intrx.mc.pet$OtherAff + intrx.mc.pet$Aggression + intrx.mc.pet$NonInt + intrx.mc.pet$Abnormal + intrx.mc.pet$Travel + intrx.mc.pet$Inactive

#total MC semcnds
intrx.mc.pet$total.mc = intrx.mc.pet$SocialPlayMC + intrx.mc.pet$GroomMC + intrx.mc.pet$OtherAffMC + intrx.mc.pet$AggressionMC + intrx.mc.pet$NonIntMC + intrx.mc.pet$AbnormalMC + intrx.mc.pet$TravelMC + intrx.mc.pet$InactiveMC

#calculate proportions intrx
intrx.mc.pet$SocialPlay.p = intrx.mc.pet$SocialPlay/intrx.mc.pet$total.intrx
intrx.mc.pet$Aggression.p = intrx.mc.pet$Aggression/intrx.mc.pet$total.intrx

#calculate proportions mc
intrx.mc.pet$SocialPlayMC.p = intrx.mc.pet$SocialPlayMC/intrx.mc.pet$total.intrx
intrx.mc.pet$AggressionMC.p = intrx.mc.pet$AggressionMC/intrx.mc.pet$total.intrx


t.test(intrx.mc.pet$SocialPlay, intrx.mc.pet$SocialPlayMC, paired=T) 
t.test(intrx.mc.pet$Aggression, intrx.mc.pet$AggressionMC, paired=T)


###post-hoc tests for differences between intrx and CO based on condition

#load and process data
intrx.mc.cb = read.table(file = "intrx-MCcb.csv", header = T, sep = ",")
str(intrx.mc.cb)
names(intrx.mc.cb)

#total intrx semcnds
intrx.mc.cb$total.intrx = intrx.mc.cb$SocialPlay + intrx.mc.cb$Groom + intrx.mc.cb$OtherAff + intrx.mc.cb$Aggression + intrx.mc.cb$NonInt + intrx.mc.cb$Abnormal + intrx.mc.cb$Travel + intrx.mc.cb$Inactive

#total MC semcnds
intrx.mc.cb$total.mc = intrx.mc.cb$SocialPlayMC + intrx.mc.cb$GroomMC + intrx.mc.cb$OtherAffMC + intrx.mc.cb$AggressionMC + intrx.mc.cb$NonIntMC + intrx.mc.cb$AbnormalMC + intrx.mc.cb$TravelMC + intrx.mc.cb$InactiveMC

#calculate proportions intrx
intrx.mc.cb$OtherAff.p = intrx.mc.cb$OtherAff/intrx.mc.cb$total.intrx

#calculate proportions MC
intrx.mc.cb$OtherAffMC.p = intrx.mc.cb$OtherAffMC/intrx.mc.cb$total.mc

t.test(intrx.mc.cb$OtherAff.p, intrx.mc.cb$OtherAffMC.p, paired=T)


intrx.mc.hb = read.table(file = "intrx-MChb.csv", header = T, sep = ",")
str(intrx.mc.hb)
names(intrx.mc.hb)

#total intrx semcnds
intrx.mc.hb$total.intrx = intrx.mc.hb$SocialPlay + intrx.mc.hb$Groom + intrx.mc.hb$OtherAff + intrx.mc.hb$Aggression + intrx.mc.hb$NonInt + intrx.mc.hb$Abnormal + intrx.mc.hb$Travel + intrx.mc.hb$Inactive

#total CO semcnds
intrx.mc.hb$total.mc = intrx.mc.hb$SocialPlayMC + intrx.mc.hb$GroomMC + intrx.mc.hb$OtherAffMC + intrx.mc.hb$AggressionMC + intrx.mc.hb$NonIntMC + intrx.mc.hb$AbnormalMC + intrx.mc.hb$TravelMC + intrx.mc.hb$InactiveMC

#calculate proportions intrx
intrx.mc.hb$OtherAff.p = intrx.mc.hb$OtherAff/intrx.mc.hb$total.intrx

#calculate proportions mc
intrx.mc.hb$OtherAffMC.p = intrx.mc.hb$OtherAffMC/intrx.mc.hb$total.mc

t.test(intrx.mc.hb$OtherAff.p, intrx.mc.hb$OtherAffMC.p, paired=T)







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

###post-hoc tests for differences between intrx and MC based on life history?

#load and process data
intrx.mc.mom = read.table(file = "intrx-MCmom.csv", header = T, sep = ",")
str(intrx.mc.mom)
names(intrx.mc.mom)

#total intrx semcnds
intrx.mc.mom$total.intrx = intrx.mc.mom$SocialPlay + intrx.mc.mom$Groom + intrx.mc.mom$OtherAff + intrx.mc.mom$Aggression + intrx.mc.mom$NonInt + intrx.mc.mom$Abnormal + intrx.mc.mom$Travel + intrx.mc.mom$Inactive

#total MC semcnds
intrx.mc.mom$total.mc = intrx.mc.mom$SocialPlayMC + intrx.mc.mom$GroomMC + intrx.mc.mom$OtherAffMC + intrx.mc.mom$AggressionMC + intrx.mc.mom$NonIntMC + intrx.mc.mom$AbnormalMC + intrx.mc.mom$TravelMC + intrx.mc.mom$InactiveMC

#calculate proportions intrx
intrx.mc.mom$SocialPlay.p = intrx.mc.mom$SocialPlay/intrx.mc.mom$total.intrx
intrx.mc.mom$Aggression.p = intrx.mc.mom$Aggression/intrx.mc.mom$total.intrx

#calculate proportions mc
intrx.mc.mom$SocialPlayMC.p = intrx.mc.mom$SocialPlayMC/intrx.mc.mom$total.intrx
intrx.mc.mom$AggressionMC.p = intrx.mc.mom$AggressionMC/intrx.mc.mom$total.intrx

t.test(intrx.mc.mom$SocialPlay, intrx.mc.mom$SocialPlayMC, paired=T)
t.test(intrx.mc.mom$Aggression, intrx.mc.mom$AggressionMC, paired=T)


intrx.mc.early = read.table(file = "intrx-MCearly.csv", header = T, sep = ",")
str(intrx.mc.early)
names(intrx.mc.early)

#total intrx semcnds
intrx.mc.early$total.intrx = intrx.mc.early$SocialPlay + intrx.mc.early$Groom + intrx.mc.early$OtherAff + intrx.mc.early$Aggression + intrx.mc.early$NonInt + intrx.mc.early$Abnormal + intrx.mc.early$Travel + intrx.mc.early$Inactive

#total MC semcnds
intrx.mc.early$total.mc = intrx.mc.early$SocialPlayMC + intrx.mc.early$GroomMC + intrx.mc.early$OtherAffMC + intrx.mc.early$AggressionMC + intrx.mc.early$NonIntMC + intrx.mc.early$AbnormalMC + intrx.mc.early$TravelMC + intrx.mc.early$InactiveMC

#calculate proportions intrx
intrx.mc.early$SocialPlay.p = intrx.mc.early$SocialPlay/intrx.mc.early$total.intrx
intrx.mc.early$Aggression.p = intrx.mc.early$Aggression/intrx.mc.early$total.intrx

#calculate proportions mc
intrx.mc.early$SocialPlayMC.p = intrx.mc.early$SocialPlayMC/intrx.mc.early$total.intrx
intrx.mc.early$AggressionMC.p = intrx.mc.early$AggressionMC/intrx.mc.early$total.intrx


t.test(intrx.mc.early$SocialPlay, intrx.mc.early$SocialPlayMC, paired=T) 
t.test(intrx.mc.early$Aggression, intrx.mc.early$AggressionMC, paired=T)


intrx.mc.late = read.table(file = "intrx-MClate.csv", header = T, sep = ",")
str(intrx.mc.late)
names(intrx.mc.late)

#total intrx semcnds
intrx.mc.late$total.intrx = intrx.mc.late$SocialPlay + intrx.mc.late$Groom + intrx.mc.late$OtherAff + intrx.mc.late$Aggression + intrx.mc.late$NonInt + intrx.mc.late$Abnormal + intrx.mc.late$Travel + intrx.mc.late$Inactive

#total MC semcnds
intrx.mc.late$total.mc = intrx.mc.late$SocialPlayMC + intrx.mc.late$GroomMC + intrx.mc.late$OtherAffMC + intrx.mc.late$AggressionMC + intrx.mc.late$NonIntMC + intrx.mc.late$AbnormalMC + intrx.mc.late$TravelMC + intrx.mc.late$InactiveMC

#calculate proportions intrx
intrx.mc.late$SocialPlay.p = intrx.mc.late$SocialPlay/intrx.mc.late$total.intrx
intrx.mc.late$Aggression.p = intrx.mc.late$Aggression/intrx.mc.late$total.intrx

#calculate proportions mc
intrx.mc.late$SocialPlayMC.p = intrx.mc.late$SocialPlayMC/intrx.mc.late$total.intrx
intrx.mc.late$AggressionMC.p = intrx.mc.late$AggressionMC/intrx.mc.late$total.intrx


t.test(intrx.mc.late$SocialPlay, intrx.mc.late$SocialPlayMC, paired=T) 
t.test(intrx.mc.late$Aggression, intrx.mc.late$AggressionMC, paired=T)


intrx.mc.nurse = read.table(file = "intrx-MCnurse.csv", header = T, sep = ",")
str(intrx.mc.nurse)
names(intrx.mc.nurse)

#total intrx semcnds
intrx.mc.nurse$total.intrx = intrx.mc.nurse$SocialPlay + intrx.mc.nurse$Groom + intrx.mc.nurse$OtherAff + intrx.mc.nurse$Aggression + intrx.mc.nurse$NonInt + intrx.mc.nurse$Abnormal + intrx.mc.nurse$Travel + intrx.mc.nurse$Inactive

#total MC semcnds
intrx.mc.nurse$total.mc = intrx.mc.nurse$SocialPlayMC + intrx.mc.nurse$GroomMC + intrx.mc.nurse$OtherAffMC + intrx.mc.nurse$AggressionMC + intrx.mc.nurse$NonIntMC + intrx.mc.nurse$AbnormalMC + intrx.mc.nurse$TravelMC + intrx.mc.nurse$InactiveMC

#calculate proportions intrx
intrx.mc.nurse$SocialPlay.p = intrx.mc.nurse$SocialPlay/intrx.mc.nurse$total.intrx
intrx.mc.nurse$Aggression.p = intrx.mc.nurse$Aggression/intrx.mc.nurse$total.intrx

#calculate proportions mc
intrx.mc.nurse$SocialPlayMC.p = intrx.mc.nurse$SocialPlayMC/intrx.mc.nurse$total.intrx
intrx.mc.nurse$AggressionMC.p = intrx.mc.nurse$AggressionMC/intrx.mc.nurse$total.intrx


t.test(intrx.mc.nurse$SocialPlay, intrx.mc.nurse$SocialPlayMC, paired=T) 
t.test(intrx.mc.nurse$Aggression, intrx.mc.nurse$AggressionMC, paired=T)


intrx.mc.pet = read.table(file = "intrx-MCpet.csv", header = T, sep = ",")
str(intrx.mc.pet)
names(intrx.mc.pet)

#total intrx semcnds
intrx.mc.pet$total.intrx = intrx.mc.pet$SocialPlay + intrx.mc.pet$Groom + intrx.mc.pet$OtherAff + intrx.mc.pet$Aggression + intrx.mc.pet$NonInt + intrx.mc.pet$Abnormal + intrx.mc.pet$Travel + intrx.mc.pet$Inactive

#total MC semcnds
intrx.mc.pet$total.mc = intrx.mc.pet$SocialPlayMC + intrx.mc.pet$GroomMC + intrx.mc.pet$OtherAffMC + intrx.mc.pet$AggressionMC + intrx.mc.pet$NonIntMC + intrx.mc.pet$AbnormalMC + intrx.mc.pet$TravelMC + intrx.mc.pet$InactiveMC

#calculate proportions intrx
intrx.mc.pet$SocialPlay.p = intrx.mc.pet$SocialPlay/intrx.mc.pet$total.intrx
intrx.mc.pet$Aggression.p = intrx.mc.pet$Aggression/intrx.mc.pet$total.intrx

#calculate proportions mc
intrx.mc.pet$SocialPlayMC.p = intrx.mc.pet$SocialPlayMC/intrx.mc.pet$total.intrx
intrx.mc.pet$AggressionMC.p = intrx.mc.pet$AggressionMC/intrx.mc.pet$total.intrx


t.test(intrx.mc.pet$SocialPlay, intrx.mc.pet$SocialPlayMC, paired=T) 
t.test(intrx.mc.pet$Aggression, intrx.mc.pet$AggressionMC, paired=T)


###post-hoc tests for differences between intrx and CO based on condition

#load and process data
intrx.mc.cb = read.table(file = "intrx-MCcb.csv", header = T, sep = ",")
str(intrx.mc.cb)
names(intrx.mc.cb)

#total intrx semcnds
intrx.mc.cb$total.intrx = intrx.mc.cb$SocialPlay + intrx.mc.cb$Groom + intrx.mc.cb$OtherAff + intrx.mc.cb$Aggression + intrx.mc.cb$NonInt + intrx.mc.cb$Abnormal + intrx.mc.cb$Travel + intrx.mc.cb$Inactive

#total MC semcnds
intrx.mc.cb$total.mc = intrx.mc.cb$SocialPlayMC + intrx.mc.cb$GroomMC + intrx.mc.cb$OtherAffMC + intrx.mc.cb$AggressionMC + intrx.mc.cb$NonIntMC + intrx.mc.cb$AbnormalMC + intrx.mc.cb$TravelMC + intrx.mc.cb$InactiveMC

#calculate proportions intrx
intrx.mc.cb$OtherAff.p = intrx.mc.cb$OtherAff/intrx.mc.cb$total.intrx

#calculate proportions MC
intrx.mc.cb$OtherAffMC.p = intrx.mc.cb$OtherAffMC/intrx.mc.cb$total.mc

t.test(intrx.mc.cb$OtherAff.p, intrx.mc.cb$OtherAffMC.p, paired=T)


intrx.mc.hb = read.table(file = "intrx-MChb.csv", header = T, sep = ",")
str(intrx.mc.hb)
names(intrx.mc.hb)

#total intrx semcnds
intrx.mc.hb$total.intrx = intrx.mc.hb$SocialPlay + intrx.mc.hb$Groom + intrx.mc.hb$OtherAff + intrx.mc.hb$Aggression + intrx.mc.hb$NonInt + intrx.mc.hb$Abnormal + intrx.mc.hb$Travel + intrx.mc.hb$Inactive

#total CO semcnds
intrx.mc.hb$total.mc = intrx.mc.hb$SocialPlayMC + intrx.mc.hb$GroomMC + intrx.mc.hb$OtherAffMC + intrx.mc.hb$AggressionMC + intrx.mc.hb$NonIntMC + intrx.mc.hb$AbnormalMC + intrx.mc.hb$TravelMC + intrx.mc.hb$InactiveMC

#calculate proportions intrx
intrx.mc.hb$OtherAff.p = intrx.mc.hb$OtherAff/intrx.mc.hb$total.intrx

#calculate proportions mc
intrx.mc.hb$OtherAffMC.p = intrx.mc.hb$OtherAffMC/intrx.mc.hb$total.mc

t.test(intrx.mc.hb$OtherAff.p, intrx.mc.hb$OtherAffMC.p, paired=T)



