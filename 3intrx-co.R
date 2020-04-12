#3-4-20
#EAP
#interaction and carry over

#load and process data
intrx.co = read.table(file = "3intrx-CO.csv", header = T, sep = ",")
str(intrx.co)
names(intrx.co)

#total intrx seconds
intrx.co$total.intrx = intrx.co$SocialPlay + intrx.co$Groom + intrx.co$OtherAff + intrx.co$Aggression + intrx.co$NonInt + intrx.co$Abnormal + intrx.co$Travel + intrx.co$Inactive

#total CO seconds
intrx.co$total.co = intrx.co$SocialPlayCO + intrx.co$GroomCO + intrx.co$OtherAffCO + intrx.co$AggressionCO + intrx.co$NonIntCO + intrx.co$AbnormalCO + intrx.co$TravelCO + intrx.co$InactiveCO

#calculate proportions intrx
intrx.co$SocialPlay.p = intrx.co$SocialPlay/intrx.co$total.intrx
intrx.co$Groom.p = intrx.co$Groom/intrx.co$total.intrx
intrx.co$OtherAff.p = intrx.co$OtherAff/intrx.co$total.intrx
intrx.co$Aggression.p = intrx.co$Aggression/intrx.co$total.intrx
intrx.co$NonInt.p = intrx.co$NonInt/intrx.co$total.intrx
intrx.co$Abnormal.p = intrx.co$Abnormal/intrx.co$total.intrx
intrx.co$Travel.p = intrx.co$Travel/intrx.co$total.intrx
intrx.co$Inactive.p = intrx.co$Inactive/intrx.co$total.intrx



#calculate proportions co
intrx.co$SocialPlayCO.p = intrx.co$SocialPlayCO/intrx.co$total.intrx
intrx.co$GroomCO.p = intrx.co$GroomCO/intrx.co$total.intrx
intrx.co$OtherAffCO.p = intrx.co$OtherAffCO/intrx.co$total.intrx
intrx.co$AggressionCO.p = intrx.co$AggressionCO/intrx.co$total.intrx
intrx.co$NonIntCO.p = intrx.co$NonIntCO/intrx.co$total.intrx
intrx.co$AbnormalCO.p = intrx.co$AbnormalCO/intrx.co$total.intrx
intrx.co$TravelCO.p = intrx.co$TravelCO/intrx.co$total.intrx
intrx.co$InactiveCO.p = intrx.co$InactiveCO/intrx.co$total.intrx


#calculate difference of intrx  and CO
intrx.co$SocialPlay.d = intrx.co$SocialPlay.p - intrx.co$SocialPlayCO.p
intrx.co$Groom.d = intrx.co$Groom.p - intrx.co$GroomCO.p
intrx.co$OtherAff.d = intrx.co$OtherAff.p - intrx.co$OtherAffCO.p
intrx.co$Aggression.d = intrx.co$Aggression.p - intrx.co$AggressionCO.p
intrx.co$NonInt.d = intrx.co$NonInt.p - intrx.co$NonIntCO.p
intrx.co$Abnormal.d = intrx.co$Abnormal.p - intrx.co$AbnormalCO.p
intrx.co$Travel.d = intrx.co$Travel.p - intrx.co$TravelCO.p
intrx.co$Inactive.d = intrx.co$Inactive.p - intrx.co$InactiveCO.p


#condense dependent variables
y <- cbind(intrx.co$SocialPlay.d, intrx.co$Groom.d, intrx.co$OtherAff.d, intrx.co$Aggression.d, intrx.co$NonInt.d, intrx.co$Abnormal.d, intrx.co$Travel.d, intrx.co$Inactive.d)#combines dependent variables

#run manova
manova(y ~ Condition * Life + Chimp, data=intrx.co, na.action=na.omit)
M1 <- manova(y ~ Condition * Life + Chimp, data=intrx.co, na.action=na.omit)
summary(M1, tol=0)
summary.aov(M1)


#try a different way to analyze
t.test(intrx.co$SocialPlay.p, intrx.co$SocialPlayCO.p, paired=T) 
t.test(intrx.co$Groom.p, intrx.co$GroomCO.p, paired=T) 
t.test(intrx.co$OtherAff.p, intrx.co$OtherAffCO.p, paired=T)
t.test(intrx.co$Aggression.p, intrx.co$AggressionCO.p, paired=T) 
t.test(intrx.co$NonInt.p, intrx.co$NonIntCO.p, paired=T) 
t.test(intrx.co$Abnormal.p, intrx.co$AbnormalCO.p, paired=T) 
t.test(intrx.co$Travel.p, intrx.co$TravelCO.p, paired=T) 
t.test(intrx.co$Inactive.p, intrx.co$InactiveCO.p, paired=T) #

###post-hoc tests for differences between intrx and CO based on life history

#load and process data
intrx.co.mom = read.table(file = "intrx-COmom.csv", header = T, sep = ",")
str(intrx.co.mom)
names(intrx.co.mom)

#total intrx seconds
intrx.co.mom$total.intrx = intrx.co.mom$SocialPlay + intrx.co.mom$Groom + intrx.co.mom$OtherAff + intrx.co.mom$Aggression + intrx.co.mom$NonInt + intrx.co.mom$Abnormal + intrx.co.mom$Travel + intrx.co.mom$Inactive

#total CO seconds
intrx.co.mom$total.co = intrx.co.mom$SocialPlayCO + intrx.co.mom$GroomCO + intrx.co.mom$OtherAffCO + intrx.co.mom$AggressionCO + intrx.co.mom$NonIntCO + intrx.co.mom$AbnormalCO + intrx.co.mom$TravelCO + intrx.co.mom$InactiveCO

#calculate proportions intrx
intrx.co.mom$SocialPlay.p = intrx.co.mom$SocialPlay/intrx.co.mom$total.intrx
intrx.co.mom$Aggression.p = intrx.co.mom$Aggression/intrx.co.mom$total.intrx
intrx.co.mom$Abnormal.p = intrx.co.mom$Abnormal/intrx.co.mom$total.intrx
intrx.co.mom$Inactive.p = intrx.co.mom$Inactive/intrx.co.mom$total.intrx


#calculate proportions co
intrx.co.mom$SocialPlayCO.p = intrx.co.mom$SocialPlayCO/intrx.co.mom$total.intrx
intrx.co.mom$AggressionCO.p = intrx.co.mom$AggressionCO/intrx.co.mom$total.intrx
intrx.co.mom$AbnormalCO.p = intrx.co.mom$AbnormalCO/intrx.co.mom$total.intrx
intrx.co.mom$InactiveCO.p = intrx.co.mom$InactiveCO/intrx.co.mom$total.intrx

#t-tests
t.test(intrx.co.mom$SocialPlay.p, intrx.co.mom$SocialPlayCO.p, paired=T)
t.test(intrx.co.mom$Aggression.p, intrx.co.mom$AggressionCO.p, paired=T)
t.test(intrx.co.mom$Abnormal.p, intrx.co.mom$AbnormalCO.p, paired=T)
t.test(intrx.co.mom$Inactive.p, intrx.co.mom$InactiveCO.p, paired=T)

#load and process data
intrx.co.late = read.table(file = "intrx-COlate.csv", header = T, sep = ",")
str(intrx.co.late)
names(intrx.co.late)

#total intrx seconds
intrx.co.late$total.intrx = intrx.co.late$SocialPlay + intrx.co.late$Groom + intrx.co.late$OtherAff + intrx.co.late$Aggression + intrx.co.late$NonInt + intrx.co.late$Abnormal + intrx.co.late$Travel + intrx.co.late$Inactive

#total CO seconds
intrx.co.late$total.co = intrx.co.late$SocialPlayCO + intrx.co.late$GroomCO + intrx.co.late$OtherAffCO + intrx.co.late$AggressionCO + intrx.co.late$NonIntCO + intrx.co.late$AbnormalCO + intrx.co.late$TravelCO + intrx.co.late$InactiveCO

#calculate proportions intrx
intrx.co.late$SocialPlay.p = intrx.co.late$SocialPlay/intrx.co.late$total.intrx
intrx.co.late$Aggression.p = intrx.co.late$Aggression/intrx.co.late$total.intrx
intrx.co.late$Abnormal.p = intrx.co.late$Abnormal/intrx.co.late$total.intrx
intrx.co.late$Inactive.p = intrx.co.late$Inactive/intrx.co.late$total.intrx


#calculate proportions co
intrx.co.late$SocialPlayCO.p = intrx.co.late$SocialPlayCO/intrx.co.late$total.intrx
intrx.co.late$AggressionCO.p = intrx.co.late$AggressionCO/intrx.co.late$total.intrx
intrx.co.late$AbnormalCO.p = intrx.co.late$AbnormalCO/intrx.co.late$total.intrx
intrx.co.late$InactiveCO.p = intrx.co.late$InactiveCO/intrx.co.late$total.intrx

#t-tests
t.test(intrx.co.late$SocialPlay.p, intrx.co.late$SocialPlayCO.p, paired=T) 
t.test(intrx.co.late$Aggression.p, intrx.co.late$AggressionCO.p, paired=T)
t.test(intrx.co.late$Abnormal.p, intrx.co.late$AbnormalCO.p, paired=T)
t.test(intrx.co.late$Inactive.p, intrx.co.late$InactiveCO.p, paired=T)

#load and process data
intrx.co.early = read.table(file = "intrx-COearly.csv", header = T, sep = ",")
str(intrx.co.early)
names(intrx.co.early)

#total intrx seconds
intrx.co.early$total.intrx = intrx.co.early$SocialPlay + intrx.co.early$Groom + intrx.co.early$OtherAff + intrx.co.early$Aggression + intrx.co.early$NonInt + intrx.co.early$Abnormal + intrx.co.early$Travel + intrx.co.early$Inactive

#total CO seconds
intrx.co.early$total.co = intrx.co.early$SocialPlayCO + intrx.co.early$GroomCO + intrx.co.early$OtherAffCO + intrx.co.early$AggressionCO + intrx.co.early$NonIntCO + intrx.co.early$AbnormalCO + intrx.co.early$TravelCO + intrx.co.early$InactiveCO

#calculate proportions intrx
intrx.co.early$SocialPlay.p = intrx.co.early$SocialPlay/intrx.co.early$total.intrx
intrx.co.early$Aggression.p = intrx.co.early$Aggression/intrx.co.early$total.intrx
intrx.co.early$Abnormal.p = intrx.co.early$Abnormal/intrx.co.early$total.intrx
intrx.co.early$Inactive.p = intrx.co.early$Inactive/intrx.co.early$total.intrx


#calculate proportions co
intrx.co.early$SocialPlayCO.p = intrx.co.early$SocialPlayCO/intrx.co.early$total.intrx
intrx.co.early$AggressionCO.p = intrx.co.early$AggressionCO/intrx.co.early$total.intrx
intrx.co.early$AbnormalCO.p = intrx.co.early$AbnormalCO/intrx.co.early$total.intrx
intrx.co.early$InactiveCO.p = intrx.co.early$InactiveCO/intrx.co.early$total.intrx

#t-tests
t.test(intrx.co.early$SocialPlay.p, intrx.co.early$SocialPlayCO.p, paired=T) 
t.test(intrx.co.early$Aggression.p, intrx.co.early$AggressionCO.p, paired=T)
t.test(intrx.co.early$Abnormal.p, intrx.co.early$AbnormalCO.p, paired=T)
t.test(intrx.co.early$Inactive.p, intrx.co.early$InactiveCO.p, paired=T)

#load and process data
intrx.co.nurse = read.table(file = "intrx-COnurse.csv", header = T, sep = ",")
str(intrx.co.nurse)
names(intrx.co.nurse)

#total intrx seconds
intrx.co.nurse$total.intrx = intrx.co.nurse$SocialPlay + intrx.co.nurse$Groom + intrx.co.nurse$OtherAff + intrx.co.nurse$Aggression + intrx.co.nurse$NonInt + intrx.co.nurse$Abnormal + intrx.co.nurse$Travel + intrx.co.nurse$Inactive

#total CO seconds
intrx.co.nurse$total.co = intrx.co.nurse$SocialPlayCO + intrx.co.nurse$GroomCO + intrx.co.nurse$OtherAffCO + intrx.co.nurse$AggressionCO + intrx.co.nurse$NonIntCO + intrx.co.nurse$AbnormalCO + intrx.co.nurse$TravelCO + intrx.co.nurse$InactiveCO

#calculate proportions intrx
intrx.co.nurse$SocialPlay.p = intrx.co.nurse$SocialPlay/intrx.co.nurse$total.intrx
intrx.co.nurse$Aggression.p = intrx.co.nurse$Aggression/intrx.co.nurse$total.intrx
intrx.co.nurse$Abnormal.p = intrx.co.nurse$Abnormal/intrx.co.nurse$total.intrx
intrx.co.nurse$Inactive.p = intrx.co.nurse$Inactive/intrx.co.nurse$total.intrx


#calculate proportions co
intrx.co.nurse$SocialPlayCO.p = intrx.co.nurse$SocialPlayCO/intrx.co.nurse$total.intrx
intrx.co.nurse$AggressionCO.p = intrx.co.nurse$AggressionCO/intrx.co.nurse$total.intrx
intrx.co.nurse$AbnormalCO.p = intrx.co.nurse$AbnormalCO/intrx.co.nurse$total.intrx
intrx.co.nurse$InactiveCO.p = intrx.co.nurse$InactiveCO/intrx.co.nurse$total.intrx

t.test(intrx.co.nurse$SocialPlay.p, intrx.co.nurse$SocialPlayCO.p, paired=T) 
t.test(intrx.co.nurse$Aggression.p, intrx.co.nurse$AggressionCO.p, paired=T)
t.test(intrx.co.nurse$Abnormal.p, intrx.co.nurse$AbnormalCO.p, paired=T)
t.test(intrx.co.nurse$Inactive.p, intrx.co.nurse$InactiveCO.p, paired=T)

#load and process data
intrx.co.pet = read.table(file = "intrx-COpet.csv", header = T, sep = ",")
str(intrx.co.pet)
names(intrx.co.pet)

#total intrx seconds
intrx.co.pet$total.intrx = intrx.co.pet$SocialPlay + intrx.co.pet$Groom + intrx.co.pet$OtherAff + intrx.co.pet$Aggression + intrx.co.pet$NonInt + intrx.co.pet$Abnormal + intrx.co.pet$Travel + intrx.co.pet$Inactive

#total CO seconds
intrx.co.pet$total.co = intrx.co.pet$SocialPlayCO + intrx.co.pet$GroomCO + intrx.co.pet$OtherAffCO + intrx.co.pet$AggressionCO + intrx.co.pet$NonIntCO + intrx.co.pet$AbnormalCO + intrx.co.pet$TravelCO + intrx.co.pet$InactiveCO

#calculate proportions intrx
intrx.co.pet$SocialPlay.p = intrx.co.pet$SocialPlay/intrx.co.pet$total.intrx
intrx.co.pet$Aggression.p = intrx.co.pet$Aggression/intrx.co.pet$total.intrx
intrx.co.pet$Abnormal.p = intrx.co.pet$Abnormal/intrx.co.pet$total.intrx
intrx.co.pet$Inactive.p = intrx.co.pet$Inactive/intrx.co.pet$total.intrx


#calculate proportions co
intrx.co.pet$SocialPlayCO.p = intrx.co.pet$SocialPlayCO/intrx.co.pet$total.intrx
intrx.co.pet$AggressionCO.p = intrx.co.pet$AggressionCO/intrx.co.pet$total.intrx
intrx.co.pet$AbnormalCO.p = intrx.co.pet$AbnormalCO/intrx.co.pet$total.intrx
intrx.co.pet$InactiveCO.p = intrx.co.pet$InactiveCO/intrx.co.pet$total.intrx

t.test(intrx.co.pet$SocialPlay.p, intrx.co.pet$SocialPlayCO.p, paired=T) 
t.test(intrx.co.pet$Aggression.p, intrx.co.pet$AggressionCO.p, paired=T)
t.test(intrx.co.pet$Abnormal.p, intrx.co.pet$AbnormalCO.p, paired=T)
t.test(intrx.co.pet$Inactive.p, intrx.co.pet$InactiveCO.p, paired=T)

###post-hoc tests for differences between intrx and CO based on condition

#load and process data
intrx.co.cb = read.table(file = "intrx-COcb.csv", header = T, sep = ",")
str(intrx.co.cb)
names(intrx.co.cb)

#total intrx seconds
intrx.co.cb$total.intrx = intrx.co.cb$SocialPlay + intrx.co.cb$Groom + intrx.co.cb$OtherAff + intrx.co.cb$Aggression + intrx.co.cb$NonInt + intrx.co.cb$Abnormal + intrx.co.cb$Travel + intrx.co.cb$Inactive

#total CO seconds
intrx.co.cb$total.co = intrx.co.cb$SocialPlayCO + intrx.co.cb$GroomCO + intrx.co.cb$OtherAffCO + intrx.co.cb$AggressionCO + intrx.co.cb$NonIntCO + intrx.co.cb$AbnormalCO + intrx.co.cb$TravelCO + intrx.co.cb$InactiveCO

#calculate proportions intrx
intrx.co.cb$OtherAff.p = intrx.co.cb$OtherAff/intrx.co.cb$total.intrx
intrx.co.cb$Groom.p = intrx.co.cb$Groom/intrx.co.cb$total.intrx

#calculate proportions co
intrx.co.cb$OtherAffCO.p = intrx.co.cb$OtherAffCO/intrx.co.cb$total.intrx
intrx.co.cb$GroomCO.p = intrx.co.cb$GroomCO/intrx.co.cb$total.intrx

t.test(intrx.co.cb$OtherAff.p, intrx.co.cb$OtherAffCO.p, paired=T)
t.test(intrx.co.cb$Groom.p, intrx.co.cb$GroomCO.p, paired=T)


intrx.co.hb = read.table(file = "intrx-COhb.csv", header = T, sep = ",")
str(intrx.co.hb)
names(intrx.co.hb)

#total intrx seconds
intrx.co.hb$total.intrx = intrx.co.hb$SocialPlay + intrx.co.hb$Groom + intrx.co.hb$OtherAff + intrx.co.hb$Aggression + intrx.co.hb$NonInt + intrx.co.hb$Abnormal + intrx.co.hb$Travel + intrx.co.hb$Inactive

#total CO seconds
intrx.co.hb$total.co = intrx.co.hb$SocialPlayCO + intrx.co.hb$GroomCO + intrx.co.hb$OtherAffCO + intrx.co.hb$AggressionCO + intrx.co.hb$NonIntCO + intrx.co.hb$AbnormalCO + intrx.co.hb$TravelCO + intrx.co.hb$InactiveCO

#calculate proportions intrx
intrx.co.hb$OtherAff.p = intrx.co.hb$OtherAff/intrx.co.hb$total.intrx
intrx.co.hb$Groom.p = intrx.co.hb$Groom/intrx.co.hb$total.intrx

#calculate proportions co
intrx.co.hb$OtherAffCO.p = intrx.co.hb$OtherAffCO/intrx.co.hb$total.intrx
intrx.co.hb$GroomCO.p = intrx.co.hb$GroomCO/intrx.co.hb$total.intrx


t.test(intrx.co.hb$OtherAff, intrx.co.hb$OtherAffCO, paired=T)
t.test(intrx.co.hb$Groom, intrx.co.hb$GroomCO, paired=T)



#make graphics

#differences in social play based on life
x <- group_by(intrx.co, Life) %>%  # Grouping function causes subsequent functions to aggregate intrx.mc by Life
  summarize(life.mean = mean(SocialPlay.d, na.rm = TRUE), # na.rm = TRUE to remove missing values
            life.sd=sd(SocialPlay.d, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(SocialPlay.d)), # of observations, excluding NAs. 
            life.se=life.sd/sqrt(n))

ggplot(data=x, aes(x=Life, y=life.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=life.mean, ymax=life.mean+life.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Early Life History") +
  ylab(expression(Time~Difference~(Interaction-Carry~Over))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8)) +
  annotate("text", x=1, y=0.025, label="a", size=4) +
  annotate("text", x=1.8, y=0.35, label="b", size=4) 

#differences in groom based on condition
x <- group_by(intrx.co, Condition) %>%  # Grouping function causes subsequent functions to aggregate intrx.mc by Condition
  summarize(cond.mean = mean(Groom.d, na.rm = TRUE), # na.rm = TRUE to remove missing values
            cond.sd=sd(Groom.d, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(Groom.d)), # of observations, excluding NAs. 
            cond.se=cond.sd/sqrt(n))

ggplot(data=x, aes(x=Condition, y=cond.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=cond.mean, ymax=cond.mean+cond.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Condition") +
  ylab(expression(Time~Difference~(Interaction-Carry~Over))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#differences in groom based on condition
x <- group_by(intrx.co, Condition) %>%  # Grouping function causes subsequent functions to aggregate intrx.mc by Condition
  summarize(cond.mean = mean(OtherAff.d, na.rm = TRUE), # na.rm = TRUE to remove missing values
            cond.sd=sd(OtherAff.d, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(OtherAff.d)), # of observations, excluding NAs. 
            cond.se=cond.sd/sqrt(n))

ggplot(data=x, aes(x=Condition, y=cond.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=cond.mean, ymax=cond.mean+cond.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Condition") +
  ylab(expression(Time~Difference~(Interaction-Carry~Over))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#differences in aggression based on life
x <- group_by(intrx.co, Life) %>%  # Grouping function causes subsequent functions to aggregate intrx.mc by Life
  summarize(life.mean = mean(Aggression.d, na.rm = TRUE), # na.rm = TRUE to remove missing values
            life.sd=sd(Aggression.d, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(Aggression.d)), # of observations, excluding NAs. 
            life.se=life.sd/sqrt(n))

ggplot(data=x, aes(x=Life, y=life.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=life.mean, ymax=life.mean+life.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Early Life History") +
  ylab(expression(Time~Difference~(Interaction-Carry~Over))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#differences in aggression based on chimp
x <- group_by(intrx.co, Chimp) %>%  # Grouping function causes subsequent functions to aggregate intrx.mc by Life
  summarize(chimp.mean = mean(Aggression.d, na.rm = TRUE), # na.rm = TRUE to remove missing values
            chimp.sd=sd(Aggression.d, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(Aggression.d)), # of observations, excluding NAs. 
            chimp.se=chimp.sd/sqrt(n))

ggplot(data=x, aes(x=Chimp, y=chimp.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=chimp.mean, ymax=chimp.mean+chimp.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Individual Chimpanzee") +
  ylab(expression(Time~Difference~(Interaction-Carry~Over))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))


###post-hoc tests for differences between intrx and CO based on life history

#load and process data
intrx.co.mom = read.table(file = "intrx-COmom.csv", header = T, sep = ",")
str(intrx.co.mom)
names(intrx.co.mom)

#total intrx seconds
intrx.co.mom$total.intrx = intrx.co.mom$SocialPlay + intrx.co.mom$Groom + intrx.co.mom$OtherAff + intrx.co.mom$Aggression + intrx.co.mom$NonInt + intrx.co.mom$Abnormal + intrx.co.mom$Travel + intrx.co.mom$Inactive

#total CO seconds
intrx.co.mom$total.co = intrx.co.mom$SocialPlayCO + intrx.co.mom$GroomCO + intrx.co.mom$OtherAffCO + intrx.co.mom$AggressionCO + intrx.co.mom$NonIntCO + intrx.co.mom$AbnormalCO + intrx.co.mom$TravelCO + intrx.co.mom$InactiveCO

#calculate proportions intrx
intrx.co.mom$SocialPlay.p = intrx.co.mom$SocialPlay/intrx.co.mom$total.intrx
intrx.co.mom$Aggression.p = intrx.co.mom$Aggression/intrx.co.mom$total.intrx
intrx.co.mom$Abnormal.p = intrx.co.mom$Abnormal/intrx.co.mom$total.intrx
intrx.co.mom$Inactive.p = intrx.co.mom$Inactive/intrx.co.mom$total.intrx


#calculate proportions co
intrx.co.mom$SocialPlayCO.p = intrx.co.mom$SocialPlayCO/intrx.co.mom$total.intrx
intrx.co.mom$AggressionCO.p = intrx.co.mom$AggressionCO/intrx.co.mom$total.intrx
intrx.co.mom$AbnormalCO.p = intrx.co.mom$AbnormalCO/intrx.co.mom$total.intrx
intrx.co.mom$InactiveCO.p = intrx.co.mom$InactiveCO/intrx.co.mom$total.intrx

#t-tests
t.test(intrx.co.mom$SocialPlay.p, intrx.co.mom$SocialPlayCO.p, paired=T)
t.test(intrx.co.mom$Aggression.p, intrx.co.mom$AggressionCO.p, paired=T)
t.test(intrx.co.mom$Abnormal.p, intrx.co.mom$AbnormalCO.p, paired=T)
t.test(intrx.co.mom$Inactive.p, intrx.co.mom$InactiveCO.p, paired=T)

#load and process data
intrx.co.late = read.table(file = "intrx-COlate.csv", header = T, sep = ",")
str(intrx.co.late)
names(intrx.co.late)

#total intrx seconds
intrx.co.late$total.intrx = intrx.co.late$SocialPlay + intrx.co.late$Groom + intrx.co.late$OtherAff + intrx.co.late$Aggression + intrx.co.late$NonInt + intrx.co.late$Abnormal + intrx.co.late$Travel + intrx.co.late$Inactive

#total CO seconds
intrx.co.late$total.co = intrx.co.late$SocialPlayCO + intrx.co.late$GroomCO + intrx.co.late$OtherAffCO + intrx.co.late$AggressionCO + intrx.co.late$NonIntCO + intrx.co.late$AbnormalCO + intrx.co.late$TravelCO + intrx.co.late$InactiveCO

#calculate proportions intrx
intrx.co.late$SocialPlay.p = intrx.co.late$SocialPlay/intrx.co.late$total.intrx
intrx.co.late$Aggression.p = intrx.co.late$Aggression/intrx.co.late$total.intrx
intrx.co.late$Abnormal.p = intrx.co.late$Abnormal/intrx.co.late$total.intrx
intrx.co.late$Inactive.p = intrx.co.late$Inactive/intrx.co.late$total.intrx


#calculate proportions co
intrx.co.late$SocialPlayCO.p = intrx.co.late$SocialPlayCO/intrx.co.late$total.intrx
intrx.co.late$AggressionCO.p = intrx.co.late$AggressionCO/intrx.co.late$total.intrx
intrx.co.late$AbnormalCO.p = intrx.co.late$AbnormalCO/intrx.co.late$total.intrx
intrx.co.late$InactiveCO.p = intrx.co.late$InactiveCO/intrx.co.late$total.intrx

#t-tests
t.test(intrx.co.late$SocialPlay.p, intrx.co.late$SocialPlayCO.p, paired=T) 
t.test(intrx.co.late$Aggression.p, intrx.co.late$AggressionCO.p, paired=T)
t.test(intrx.co.late$Abnormal.p, intrx.co.late$AbnormalCO.p, paired=T)
t.test(intrx.co.late$Inactive.p, intrx.co.late$InactiveCO.p, paired=T)

#load and process data
intrx.co.early = read.table(file = "intrx-COearly.csv", header = T, sep = ",")
str(intrx.co.early)
names(intrx.co.early)

#total intrx seconds
intrx.co.early$total.intrx = intrx.co.early$SocialPlay + intrx.co.early$Groom + intrx.co.early$OtherAff + intrx.co.early$Aggression + intrx.co.early$NonInt + intrx.co.early$Abnormal + intrx.co.early$Travel + intrx.co.early$Inactive

#total CO seconds
intrx.co.early$total.co = intrx.co.early$SocialPlayCO + intrx.co.early$GroomCO + intrx.co.early$OtherAffCO + intrx.co.early$AggressionCO + intrx.co.early$NonIntCO + intrx.co.early$AbnormalCO + intrx.co.early$TravelCO + intrx.co.early$InactiveCO

#calculate proportions intrx
intrx.co.early$SocialPlay.p = intrx.co.early$SocialPlay/intrx.co.early$total.intrx
intrx.co.early$Aggression.p = intrx.co.early$Aggression/intrx.co.early$total.intrx
intrx.co.early$Abnormal.p = intrx.co.early$Abnormal/intrx.co.early$total.intrx
intrx.co.early$Inactive.p = intrx.co.early$Inactive/intrx.co.early$total.intrx


#calculate proportions co
intrx.co.early$SocialPlayCO.p = intrx.co.early$SocialPlayCO/intrx.co.early$total.intrx
intrx.co.early$AggressionCO.p = intrx.co.early$AggressionCO/intrx.co.early$total.intrx
intrx.co.early$AbnormalCO.p = intrx.co.early$AbnormalCO/intrx.co.early$total.intrx
intrx.co.early$InactiveCO.p = intrx.co.early$InactiveCO/intrx.co.early$total.intrx

#t-tests
t.test(intrx.co.early$SocialPlay.p, intrx.co.early$SocialPlayCO.p, paired=T) 
t.test(intrx.co.early$Aggression.p, intrx.co.early$AggressionCO.p, paired=T)
t.test(intrx.co.early$Abnormal.p, intrx.co.early$AbnormalCO.p, paired=T)
t.test(intrx.co.early$Inactive.p, intrx.co.early$InactiveCO.p, paired=T)

#load and process data
intrx.co.nurse = read.table(file = "intrx-COnurse.csv", header = T, sep = ",")
str(intrx.co.nurse)
names(intrx.co.nurse)

#total intrx seconds
intrx.co.nurse$total.intrx = intrx.co.nurse$SocialPlay + intrx.co.nurse$Groom + intrx.co.nurse$OtherAff + intrx.co.nurse$Aggression + intrx.co.nurse$NonInt + intrx.co.nurse$Abnormal + intrx.co.nurse$Travel + intrx.co.nurse$Inactive

#total CO seconds
intrx.co.nurse$total.co = intrx.co.nurse$SocialPlayCO + intrx.co.nurse$GroomCO + intrx.co.nurse$OtherAffCO + intrx.co.nurse$AggressionCO + intrx.co.nurse$NonIntCO + intrx.co.nurse$AbnormalCO + intrx.co.nurse$TravelCO + intrx.co.nurse$InactiveCO

#calculate proportions intrx
intrx.co.nurse$SocialPlay.p = intrx.co.nurse$SocialPlay/intrx.co.nurse$total.intrx
intrx.co.nurse$Aggression.p = intrx.co.nurse$Aggression/intrx.co.nurse$total.intrx
intrx.co.nurse$Abnormal.p = intrx.co.nurse$Abnormal/intrx.co.nurse$total.intrx
intrx.co.nurse$Inactive.p = intrx.co.nurse$Inactive/intrx.co.nurse$total.intrx


#calculate proportions co
intrx.co.nurse$SocialPlayCO.p = intrx.co.nurse$SocialPlayCO/intrx.co.nurse$total.intrx
intrx.co.nurse$AggressionCO.p = intrx.co.nurse$AggressionCO/intrx.co.nurse$total.intrx
intrx.co.nurse$AbnormalCO.p = intrx.co.nurse$AbnormalCO/intrx.co.nurse$total.intrx
intrx.co.nurse$InactiveCO.p = intrx.co.nurse$InactiveCO/intrx.co.nurse$total.intrx

t.test(intrx.co.nurse$SocialPlay.p, intrx.co.nurse$SocialPlayCO.p, paired=T) 
t.test(intrx.co.nurse$Aggression.p, intrx.co.nurse$AggressionCO.p, paired=T)
t.test(intrx.co.nurse$Abnormal.p, intrx.co.nurse$AbnormalCO.p, paired=T)
t.test(intrx.co.nurse$Inactive.p, intrx.co.nurse$InactiveCO.p, paired=T)

#load and process data
intrx.co.pet = read.table(file = "intrx-COpet.csv", header = T, sep = ",")
str(intrx.co.pet)
names(intrx.co.pet)

#total intrx seconds
intrx.co.pet$total.intrx = intrx.co.pet$SocialPlay + intrx.co.pet$Groom + intrx.co.pet$OtherAff + intrx.co.pet$Aggression + intrx.co.pet$NonInt + intrx.co.pet$Abnormal + intrx.co.pet$Travel + intrx.co.pet$Inactive

#total CO seconds
intrx.co.pet$total.co = intrx.co.pet$SocialPlayCO + intrx.co.pet$GroomCO + intrx.co.pet$OtherAffCO + intrx.co.pet$AggressionCO + intrx.co.pet$NonIntCO + intrx.co.pet$AbnormalCO + intrx.co.pet$TravelCO + intrx.co.pet$InactiveCO

#calculate proportions intrx
intrx.co.pet$SocialPlay.p = intrx.co.pet$SocialPlay/intrx.co.pet$total.intrx
intrx.co.pet$Aggression.p = intrx.co.pet$Aggression/intrx.co.pet$total.intrx
intrx.co.pet$Abnormal.p = intrx.co.pet$Abnormal/intrx.co.pet$total.intrx
intrx.co.pet$Inactive.p = intrx.co.pet$Inactive/intrx.co.pet$total.intrx


#calculate proportions co
intrx.co.pet$SocialPlayCO.p = intrx.co.pet$SocialPlayCO/intrx.co.pet$total.intrx
intrx.co.pet$AggressionCO.p = intrx.co.pet$AggressionCO/intrx.co.pet$total.intrx
intrx.co.pet$AbnormalCO.p = intrx.co.pet$AbnormalCO/intrx.co.pet$total.intrx
intrx.co.pet$InactiveCO.p = intrx.co.pet$InactiveCO/intrx.co.pet$total.intrx

t.test(intrx.co.pet$SocialPlay.p, intrx.co.pet$SocialPlayCO.p, paired=T) 
t.test(intrx.co.pet$Aggression.p, intrx.co.pet$AggressionCO.p, paired=T)
t.test(intrx.co.pet$Abnormal.p, intrx.co.pet$AbnormalCO.p, paired=T)
t.test(intrx.co.pet$Inactive.p, intrx.co.pet$InactiveCO.p, paired=T)

###post-hoc tests for differences between intrx and CO based on condition

#load and process data
intrx.co.cb = read.table(file = "intrx-COcb.csv", header = T, sep = ",")
str(intrx.co.cb)
names(intrx.co.cb)

#total intrx seconds
intrx.co.cb$total.intrx = intrx.co.cb$SocialPlay + intrx.co.cb$Groom + intrx.co.cb$OtherAff + intrx.co.cb$Aggression + intrx.co.cb$NonInt + intrx.co.cb$Abnormal + intrx.co.cb$Travel + intrx.co.cb$Inactive

#total CO seconds
intrx.co.cb$total.co = intrx.co.cb$SocialPlayCO + intrx.co.cb$GroomCO + intrx.co.cb$OtherAffCO + intrx.co.cb$AggressionCO + intrx.co.cb$NonIntCO + intrx.co.cb$AbnormalCO + intrx.co.cb$TravelCO + intrx.co.cb$InactiveCO

#calculate proportions intrx
intrx.co.cb$OtherAff.p = intrx.co.cb$OtherAff/intrx.co.cb$total.intrx
intrx.co.cb$Groom.p = intrx.co.cb$Groom/intrx.co.cb$total.intrx

#calculate proportions co
intrx.co.cb$OtherAffCO.p = intrx.co.cb$OtherAffCO/intrx.co.cb$total.intrx
intrx.co.cb$GroomCO.p = intrx.co.cb$GroomCO/intrx.co.cb$total.intrx

t.test(intrx.co.cb$OtherAff.p, intrx.co.cb$OtherAffCO.p, paired=T)
t.test(intrx.co.cb$Groom.p, intrx.co.cb$GroomCO.p, paired=T)


intrx.co.hb = read.table(file = "intrx-COhb.csv", header = T, sep = ",")
str(intrx.co.hb)
names(intrx.co.hb)

#total intrx seconds
intrx.co.hb$total.intrx = intrx.co.hb$SocialPlay + intrx.co.hb$Groom + intrx.co.hb$OtherAff + intrx.co.hb$Aggression + intrx.co.hb$NonInt + intrx.co.hb$Abnormal + intrx.co.hb$Travel + intrx.co.hb$Inactive

#total CO seconds
intrx.co.hb$total.co = intrx.co.hb$SocialPlayCO + intrx.co.hb$GroomCO + intrx.co.hb$OtherAffCO + intrx.co.hb$AggressionCO + intrx.co.hb$NonIntCO + intrx.co.hb$AbnormalCO + intrx.co.hb$TravelCO + intrx.co.hb$InactiveCO

#calculate proportions intrx
intrx.co.hb$OtherAff.p = intrx.co.hb$OtherAff/intrx.co.hb$total.intrx
intrx.co.hb$Groom.p = intrx.co.hb$Groom/intrx.co.hb$total.intrx

#calculate proportions co
intrx.co.hb$OtherAffCO.p = intrx.co.hb$OtherAffCO/intrx.co.hb$total.intrx
intrx.co.hb$GroomCO.p = intrx.co.hb$GroomCO/intrx.co.hb$total.intrx


t.test(intrx.co.hb$OtherAff, intrx.co.hb$OtherAffCO, paired=T)
t.test(intrx.co.hb$Groom, intrx.co.hb$GroomCO, paired=T)
