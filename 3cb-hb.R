#CB vs HB
#EAP
#3-4-20


#test of differences in behavior within the interaction session
#load and process data
intrx = read.table(file = "intrx.csv", header = T, sep = ",")
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


