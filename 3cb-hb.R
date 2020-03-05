#CB vs HB
#EAP
#3-4-20

#load and process data
cb.hb = read.table(file = "3CB-HB.csv", header = T, sep = ",")
str(cb.hb)
names(cb.hb)


#total cb seconds
cb.hb$total.cb = cb.hb$SocialPlayCB + cb.hb$GroomCB + cb.hb$OtherAffCB + cb.hb$AggressionCB + cb.hb$NonIntCB + cb.hb$AbnormalCB + cb.hb$TravelCB + cb.hb$InactiveCB

#total hb seconds
cb.hb$total.hb = cb.hb$SocialPlayHB + cb.hb$GroomHB + cb.hb$OtherAffHB + cb.hb$AggressionHB + cb.hb$NonIntHB + cb.hb$AbnormalHB + cb.hb$TravelHB + cb.hb$InactiveHB

#calculate proportions cb
cb.hb$SocialPlayCB.p = cb.hb$SocialPlayCB/cb.hb$total.cb
cb.hb$GroomCB.p = cb.hb$GroomCB/cb.hb$total.cb
cb.hb$OtherAffCB.p = cb.hb$OtherAffCB/cb.hb$total.cb
cb.hb$AggressionCB.p = cb.hb$AggressionCB/cb.hb$total.cb
cb.hb$NonIntCB.p = cb.hb$NonIntCB/cb.hb$total.cb
cb.hb$AbnormalCB.p = cb.hb$AbnormalCB/cb.hb$total.cb
cb.hb$TravelCB.p = cb.hb$TravelCB/cb.hb$total.cb
cb.hb$InactiveCB.p = cb.hb$InactiveCB/cb.hb$total.cb



#calculate proportions hb
cb.hb$SocialPlayHB.p = cb.hb$SocialPlayHB/cb.hb$total.hb
cb.hb$GroomHB.p = cb.hb$GroomHB/cb.hb$total.hb
cb.hb$OtherAffHB.p = cb.hb$OtherAffHB/cb.hb$total.hb
cb.hb$AggressionHB.p = cb.hb$AggressionHB/cb.hb$total.hb
cb.hb$NonIntHB.p = cb.hb$NonIntHB/cb.hb$total.hb
cb.hb$AbnormalHB.p = cb.hb$AbnormalHB/cb.hb$total.hb
cb.hb$TravelHB.p = cb.hb$TravelHB/cb.hb$total.hb
cb.hb$InactiveHB.p = cb.hb$InactiveHB/cb.hb$total.hb


#run paired t-tests to find difference between interaction and matched control
#paired t-test of interaction v matched control for each behavior, p = 0.007
t.test(cb.hb$SocialPlayCB.p, cb.hb$SocialPlayHB.p, paired=T) 
t.test(cb.hb$GroomCB.p, cb.hb$GroomHB.p, paired=T) 
t.test(cb.hb$OtherAffCB.p, cb.hb$OtherAffHB.p, paired=T) 
t.test(cb.hb$AggressionCB.p, cb.hb$AggressionHB.p, paired=T)
t.test(cb.hb$NonIntCB.p, cb.hb$NonIntHB.p, paired=T) 
t.test(cb.hb$AbnormalCB.p, cb.hb$AbnormalHB.p, paired=T) 
t.test(cb.hb$TravelCB.p, cb.hb$TravelHB.p, paired=T)
t.test(cb.hb$InactiveCB.p, cb.hb$InactiveHB.p, paired=T)



#test of differences in behavior within the interaction session
#load and process data
intrx = read.table(file = "3intrx-MC.csv", header = T, sep = ",")
str(intrx)
names(intrx)


aov(SocialPlay ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A1 <- aov(SocialPlay ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A1,tol=0)

aov(Groom ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A2 <- aov(Groom ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A2,tol=0)

aov(OtherAff ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A3 <- aov(OtherAff ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A3,tol=0)

aov(Aggression ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A4 <- aov(Aggression ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A4,tol=0)

aov(NonInt ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A5 <- aov(NonInt ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A5,tol=0)

aov(Abnormal ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A6 <- aov(Abnormal ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A6,tol=0)

aov(Travel ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A7 <- aov(Travel ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A7,tol=0)

aov(Inactive ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
A8 <- aov(Inactive ~ Condition * Life + Chimp, data=intrx, na.action=na.omit)
summary(A8,tol=0)

#calculate differences in proportion for social play, groom


#make graphics
#Summarize SocialPlay by life history
x <- group_by(intrx, Life) %>%  # Grouping function causes subsequent functions to aggregate intrx by Condition
  summarize(life.mean = mean(SocialPlay, na.rm = TRUE), # na.rm = TRUE to remove missing values
            life.sd=sd(SocialPlay, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(SocialPlay)), # of observations, excluding NAs. 
            life.se=life.sd/sqrt(n))

ggplot(data=x, aes(x=Life, y=life.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=life.mean, ymax=life.mean+life.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Early Life History") +
  ylab(expression(Social~Play)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#Summarize Groom by condition
x <- group_by(intrx, Condition) %>%  # Grouping function causes subsequent functions to aggregate intrx by Condition
  summarize(condition.mean = mean(Groom, na.rm = TRUE), # na.rm = TRUE to remove missing values
            condition.sd=sd(Groom, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(Groom)), # of observations, excluding NAs. 
            condition.se=condition.sd/sqrt(n))

ggplot(data=x, aes(x=Condition, y=condition.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=condition.mean, ymax=condition.mean+condition.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Condition") +
  ylab(expression(Groom)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#Summarize OtherAff by condition
x <- group_by(intrx, Condition) %>%  # Grouping function causes subsequent functions to aggregate intrx by Condition
  summarize(condition.mean = mean(OtherAff, na.rm = TRUE), # na.rm = TRUE to remove missing values
            condition.sd=sd(OtherAff, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(OtherAff)), # of observations, excluding NAs. 
            condition.se=condition.sd/sqrt(n))

ggplot(data=x, aes(x=Condition, y=condition.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=condition.mean, ymax=condition.mean+condition.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Condition") +
  ylab(expression(Other~Affinitive~Social)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#Summarize Aggression by life
x <- group_by(intrx, Life) %>%  # Grouping function causes subsequent functions to aggregate intrx by Life
  summarize(condition.mean = mean(Aggression, na.rm = TRUE), # na.rm = TRUE to remove missing values
            condition.sd=sd(Aggression, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(Aggression)), # of observations, excluding NAs. 
            condition.se=condition.sd/sqrt(n))

ggplot(data=x, aes(x=Life, y=condition.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=condition.mean, ymax=condition.mean+condition.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Early Life History") +
  ylab(expression(Aggression)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#Summarize Aggression by chimp
x <- group_by(intrx, Chimp) %>%  # Grouping function causes subsequent functions to aggregate intrx by Chimp
  summarize(condition.mean = mean(Aggression, na.rm = TRUE), # na.rm = TRUE to remove missing values
            condition.sd=sd(Aggression, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(Aggression)), # of observations, excluding NAs. 
            condition.se=condition.sd/sqrt(n))

ggplot(data=x, aes(x=Chimp, y=condition.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=condition.mean, ymax=condition.mean+condition.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Individual Chimpanzee") +
  ylab(expression(Aggression)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))
