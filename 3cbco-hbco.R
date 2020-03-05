#CBCO vs HBCO
#EAP
#3-4-20

#load and process data
cbco.hbco = read.table(file = "3CBCO-HBCO.csv", header = T, sep = ",")
str(cbco.hbco)
names(cbco.hbco)


#total cb seconds
cbco.hbco$total.cb = cbco.hbco$SocialPlayCB + cbco.hbco$GroomCB + cbco.hbco$OtherAffCB + cbco.hbco$AggressionCB + cbco.hbco$NonIntCB + cbco.hbco$AbnormalCB + cbco.hbco$TravelCB + cbco.hbco$InactiveCB

#total hb seconds
cbco.hbco$total.hb = cbco.hbco$SocialPlayHB + cbco.hbco$GroomHB + cbco.hbco$OtherAffHB + cbco.hbco$AggressionHB + cbco.hbco$NonIntHB + cbco.hbco$AbnormalHB + cbco.hbco$TravelHB + cbco.hbco$InactiveHB

#calculate proportions cb
cbco.hbco$SocialPlayCB.p = cbco.hbco$SocialPlayCB/cbco.hbco$total.cb
cbco.hbco$GroomCB.p = cbco.hbco$GroomCB/cbco.hbco$total.cb
cbco.hbco$OtherAffCB.p = cbco.hbco$OtherAffCB/cbco.hbco$total.cb
cbco.hbco$AggressionCB.p = cbco.hbco$AggressionCB/cbco.hbco$total.cb
cbco.hbco$NonIntCB.p = cbco.hbco$NonIntCB/cbco.hbco$total.cb
cbco.hbco$AbnormalCB.p = cbco.hbco$AbnormalCB/cbco.hbco$total.cb
cbco.hbco$TravelCB.p = cbco.hbco$TravelCB/cbco.hbco$total.cb
cbco.hbco$InactiveCB.p = cbco.hbco$InactiveCB/cbco.hbco$total.cb



#calculate proportions hb
cbco.hbco$SocialPlayHB.p = cbco.hbco$SocialPlayHB/cbco.hbco$total.hb
cbco.hbco$GroomHB.p = cbco.hbco$GroomHB/cbco.hbco$total.hb
cbco.hbco$OtherAffHB.p = cbco.hbco$OtherAffHB/cbco.hbco$total.hb
cbco.hbco$AggressionHB.p = cbco.hbco$AggressionHB/cbco.hbco$total.hb
cbco.hbco$NonIntHB.p = cbco.hbco$NonIntHB/cbco.hbco$total.hb
cbco.hbco$AbnormalHB.p = cbco.hbco$AbnormalHB/cbco.hbco$total.hb
cbco.hbco$TravelHB.p = cbco.hbco$TravelHB/cbco.hbco$total.hb
cbco.hbco$InactiveHB.p = cbco.hbco$InactiveHB/cbco.hbco$total.hb


#run paired t-tests to find difference between interaction and matched control
#paired t-test of interaction v matched control for each behavior, p = 0.007
t.test(cbco.hbco$SocialPlayCB.p, cbco.hbco$SocialPlayHB.p, paired=T) 
t.test(cbco.hbco$GroomCB.p, cbco.hbco$GroomHB.p, paired=T) 
t.test(cbco.hbco$OtherAffCB.p, cbco.hbco$OtherAffHB.p, paired=T) 
t.test(cbco.hbco$AggressionCB.p, cbco.hbco$AggressionHB.p, paired=T)
t.test(cbco.hbco$NonIntCB.p, cbco.hbco$NonIntHB.p, paired=T) 
t.test(cbco.hbco$AbnormalCB.p, cbco.hbco$AbnormalHB.p, paired=T) 
t.test(cbco.hbco$TravelCB.p, cbco.hbco$TravelHB.p, paired=T)
t.test(cbco.hbco$InactiveCB.p, cbco.hbco$InactiveHB.p, paired=T)



#test of differences in behavior within the interaction session
#load and process data
carryover = read.table(file = "3intrx-CO.csv", header = T, sep = ",")
str(carryover)
names(carryover)


aov(SocialPlayCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A1 <- aov(SocialPlayCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A1,tol=0)

aov(GroomCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A2 <- aov(GroomCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A2,tol=0)

aov(OtherAffCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A3 <- aov(OtherAffCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A3,tol=0)

aov(AggressionCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A4 <- aov(AggressionCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A4,tol=0)

aov(NonIntCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A5 <- aov(NonIntCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A5,tol=0)

aov(AbnormalCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A6 <- aov(AbnormalCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A6,tol=0)

aov(TravelCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A7 <- aov(TravelCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A7,tol=0)

aov(InactiveCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
A8 <- aov(InactiveCO ~ Condition * Life + Chimp, data=carryover, na.action=na.omit)
summary(A8,tol=0)

#calculate differences in proportion for social play, groom


#make graphics
#Summarize aggression by life history
x <- group_by(carryover, Life) %>%  # Grouping function causes subsequent functions to aggregate carryover by life
  summarize(life.mean = mean(AggressionCO, na.rm = TRUE), # na.rm = TRUE to remove missing values
            life.sd=sd(AggressionCO, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(AggressionCO)), # of observations, excluding NAs. 
            life.se=life.sd/sqrt(n))

ggplot(data=x, aes(x=Life, y=life.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=life.mean, ymax=life.mean+life.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Early Life History") +
  ylab(expression(Aggression)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#Summarize non-interactive by condition
x <- group_by(carryover, Condition) %>%  # Grouping function causes subsequent functions to aggregate carryover by Condition
  summarize(condition.mean = mean(NonIntCO, na.rm = TRUE), # na.rm = TRUE to remove missing values
            condition.sd=sd(NonIntCO, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(NonIntCO)), # of observations, excluding NAs. 
            condition.se=condition.sd/sqrt(n))

ggplot(data=x, aes(x=Condition, y=condition.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=condition.mean, ymax=condition.mean+condition.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Condition") +
  ylab(expression(Non-Interactive)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#Summarize abnormal by life
x <- group_by(carryover, Life) %>%  # Grouping function causes subsequent functions to aggregate carryover by Life
  summarize(condition.mean = mean(AbnormalCO, na.rm = TRUE), # na.rm = TRUE to remove missing values
            condition.sd=sd(AbnormalCO, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(AbnormalCO)), # of observations, excluding NAs. 
            condition.se=condition.sd/sqrt(n))

ggplot(data=x, aes(x=Life, y=condition.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=condition.mean, ymax=condition.mean+condition.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Early Life History") +
  ylab(expression(Abnormal)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#Summarize travel by condition
x <- group_by(carryover, Condition) %>%  # Grouping function causes subsequent functions to aggregate carryover by Condition
  summarize(condition.mean = mean(TravelCO, na.rm = TRUE), # na.rm = TRUE to remove missing values
            condition.sd=sd(TravelCO, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(TravelCO)), # of observations, excluding NAs. 
            condition.se=condition.sd/sqrt(n))

ggplot(data=x, aes(x=Condition, y=condition.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=condition.mean, ymax=condition.mean+condition.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Condition") +
  ylab(expression(Travel)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#Summarize travel by life
x <- group_by(carryover, Life) %>%  # Grouping function causes subsequent functions to aggregate carryover by Life
  summarize(condition.mean = mean(TravelCO, na.rm = TRUE), # na.rm = TRUE to remove missing values
            condition.sd=sd(TravelCO, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(TravelCO)), # of observations, excluding NAs. 
            condition.se=condition.sd/sqrt(n))

ggplot(data=x, aes(x=Life, y=condition.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=condition.mean, ymax=condition.mean+condition.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Early Life History") +
  ylab(expression(Travel)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#Summarize travel by chimp
x <- group_by(carryover, Chimp) %>%  # Grouping function causes subsequent functions to aggregate carryover by Chimp
  summarize(condition.mean = mean(TravelCO, na.rm = TRUE), # na.rm = TRUE to remove missing values
            condition.sd=sd(TravelCO, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(TravelCO)), # of observations, excluding NAs. 
            condition.se=condition.sd/sqrt(n))

ggplot(data=x, aes(x=Chimp, y=condition.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=condition.mean, ymax=condition.mean+condition.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Individual Chimpanzee") +
  ylab(expression(Travel)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#Summarize inactive by chimp
x <- group_by(carryover, Chimp) %>%  # Grouping function causes subsequent functions to aggregate carryover by Chimp
  summarize(condition.mean = mean(InactiveCO, na.rm = TRUE), # na.rm = TRUE to remove missing values
            condition.sd=sd(InactiveCO, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(InactiveCO)), # of observations, excluding NAs. 
            condition.se=condition.sd/sqrt(n))

ggplot(data=x, aes(x=Chimp, y=condition.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=condition.mean, ymax=condition.mean+condition.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Individual Chimpanzee") +
  ylab(expression(Inactive)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

