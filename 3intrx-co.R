#3-4-20
#EAP
#interaction and carry over

#load and process data
intrx.co = read.table(file = "3intrx-CO.csv", header = T, sep = ",")
str(intrx.co)
names(intrx.co)

#total interaction seconds
intrx.co$total.intrx = intrx.co$SocialPlay + intrx.co$Groom + intrx.co$OtherAff + intrx.co$Aggression + intrx.co$NonInt + intrx.co$Abnormal + intrx.co$Travel + intrx.co$Inactive 

#total carry over seconds
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



#calculate proportions carry over
intrx.co$SocialPlayCO.p = intrx.co$SocialPlayCO/intrx.co$total.co
intrx.co$GroomCO.p = intrx.co$GroomCO/intrx.co$total.co
intrx.co$OtherAffCO.p = intrx.co$OtherAffCO/intrx.co$total.co
intrx.co$AggressionCO.p = intrx.co$AggressionCO/intrx.co$total.co
intrx.co$NonIntCO.p = intrx.co$NonIntCO/intrx.co$total.co
intrx.co$AbnormalCO.p = intrx.co$AbnormalCO/intrx.co$total.co
intrx.co$TravelCO.p = intrx.co$TravelCO/intrx.co$total.co
intrx.co$InactiveCO.p = intrx.co$InactiveCO/intrx.co$total.co

#calculate difference of interaction and carry over
intrx.co$SocialPlay.d = intrx.co$SocialPlay.p - intrx.co$SocialPlayCO.p
intrx.co$Groom.d = intrx.co$Groom.p - intrx.co$GroomCO.p
intrx.co$OtherAff.d = intrx.co$OtherAff.p - intrx.co$OtherAffCO.p
intrx.co$Aggression.d = intrx.co$Aggression.p - intrx.co$AggressionCO.p
intrx.co$NonInt.d = intrx.co$NonInt.p - intrx.co$NonIntCO.p
intrx.co$Abnormal.d = intrx.co$Abnormal.p - intrx.co$AbnormalCO.p
intrx.co$Travel.d = intrx.co$Travel.p - intrx.co$TravelCO.p
intrx.co$Inactive.d = intrx.co$Inactive.p - intrx.co$InactiveCO.p




#condense dependent variables
y <- cbind(intrx.co$CSP.d, intrx.co$Cgroom.d, intrx.co$COAS.d, intrx.co$SocialPlay.d, intrx.co$Groom.d, intrx.co$OtherAff.d, intrx.co$Aggression.d, intrx.co$NonInt.d, intrx.co$Abnormal.d, intrx.co$Travel.d, intrx.co$Inactive.d)#combines dependent variables

#run manova
manova(y ~ Life * Life + Chimp, data=intrx.co, na.action=na.omit)
M1 <- manova(y ~ Life * Life + Chimp, data=intrx.co, na.action=na.omit)
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
        axis.text.x=element_text(size=8))

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



#Social Play is significant, anova to analyze
AOV1 = aov(intrx.co$SocialPlay.d ~ intrx.co$Life)
summary(AOV1)
TukeyHSD(AOV1)



