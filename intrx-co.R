#2-Nov-19
#EAP
#Interaction and carry over analysis

#load and process data
intrx.co = read.table(file = "intrx-CO.csv", header = T, sep = ",")
str(intrx.co)

#total interaction seconds
intrx.co$total.intrx = intrx.co$CAF + intrx.co$HAF + intrx.co$NAS + intrx.co$Other + intrx.co$Ab + intrx.co$Tr + intrx.co$In

#total carry over seconds
intrx.co$total.co = intrx.co$CAFCO + intrx.co$HAFCO + intrx.co$NASCO + intrx.co$OtherCO + intrx.co$AbCO + intrx.co$TrCO + intrx.co$InCO

#calculate proportions intrx
intrx.co$CAF.p = intrx.co$CAF/intrx.co$total.intrx
intrx.co$HAF.p = intrx.co$HAF/intrx.co$total.intrx
intrx.co$NAS.p = intrx.co$NAS/intrx.co$total.intrx
intrx.co$Other.p = intrx.co$Other/intrx.co$total.intrx
intrx.co$Ab.p = intrx.co$Ab/intrx.co$total.intrx
intrx.co$In.p = intrx.co$In/intrx.co$total.intrx
intrx.co$Tr.p = intrx.co$Tr/intrx.co$total.intrx

#calculate proportions carry over
intrx.co$CAFCO.p = intrx.co$CAFCO/intrx.co$total.co
intrx.co$HAFCO.p = intrx.co$HAFCO/intrx.co$total.co
intrx.co$NASCO.p = intrx.co$NASCO/intrx.co$total.co
intrx.co$OtherCO.p = intrx.co$OtherCO/intrx.co$total.co
intrx.co$AbCO.p = intrx.co$AbCO/intrx.co$total.co
intrx.co$TrCO.p = intrx.co$TrCO/intrx.co$total.co
intrx.co$InCO.p = intrx.co$InCO/intrx.co$total.co

#calculate difference of interaction and carry over
intrx.co$Caf.d = intrx.co$CAF.p - intrx.co$CAFCO.p
intrx.co$Haf.d = intrx.co$HAF.p - intrx.co$HAFCO.p
intrx.co$Nas.d = intrx.co$NAS.p - intrx.co$NASCO.p
intrx.co$Other.d = intrx.co$Other.p - intrx.co$OtherCO.p
intrx.co$Ab.d = intrx.co$Ab.p - intrx.co$AbCO.p
intrx.co$Tr.d = intrx.co$Tr.p - intrx.co$TrCO.p
intrx.co$In.d = intrx.co$In.p - intrx.co$InCO.p

#graphs
hist(intrx.co$Caf.d)
hist(intrx.co$Haf.d)
hist(intrx.co$In.d)
hist(intrx.co$Nas.d)
hist(intrx.co$Other.d)
hist(intrx.co$Tr.d)
hist(intrx.co$Ab.d)



#condense dependent variables
y <- cbind(intrx.co$Caf.d, intrx.co$Haf.d, intrx.co$Nas.d, intrx.co$Other.d, intrx.co$Ab.d, intrx.co$Tr.d, intrx.co$In.d)#combines dependent variables

#run manova
manova(y ~ Condition * LIFE + Chimp, data=intrx.co, na.action=na.omit)
M1 <- manova(y ~ Condition * LIFE + Chimp, data=intrx.co, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)


#use graphics to analyze residuals
plot(residuals(M1))
qqnorm(residuals(M1))#this looks bad
qqline(residuals(M1))
hist(residuals(M1))#but this looks really good

#make graphics
x <- group_by(intrx.co, Condition) %>%  # Grouping function causes subsequent functions to aggregate intrx.mc by Condition
  summarize(cond.mean = mean(In.d, na.rm = TRUE), # na.rm = TRUE to remove missing values
            cond.sd=sd(In.d, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(In.d)), # of observations, excluding NAs. 
            cond.se=cond.sd/sqrt(n))

ggplot(data=x, aes(x=Condition, y=cond.mean)) + #data is what you plot
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=cond.mean, ymax=cond.mean+cond.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Interaction Type") +
  ylab(expression(Time~Difference~(Interaction-Carry~Over))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#response 5 = Human Affinitive is significant, anova to analyze
AOV1 = aov(intrx.co$Haf.d ~ intrx.co$LIFE)
summary(AOV1)
TukeyHSD(AOV1)

#response 7 = Nonaffinitive Social is significant, anova to analyze
AOV1 = aov(intrx.co$Nas.d ~ intrx.co$LIFE)
summary(AOV1)
TukeyHSD(AOV1)

#for both variables, slight differences between "mom" and other life history categories

#try a different way to analyze
t.test(intrx.co$HAF.p, intrx.co$HAFCO.p, paired=T) #p = 5.78e-15
t.test(intrx.co$CAF.p, intrx.co$CAFCO.p, paired=T) #p = .2749
t.test(intrx.co$NAS.p, intrx.co$NASCO.p, paired=T) #p = .01127
t.test(intrx.co$Other.p, intrx.co$OtherCO.p, paired=T) #p = 6.963e-05
t.test(intrx.co$Ab.p, intrx.co$AbCO.p, paired=T) #p = .2066
t.test(intrx.co$Tr.p, intrx.co$TrCO.p, paired=T) #p = .6283
t.test(intrx.co$In.p, intrx.co$InCO.p, paired=T) #p = 1.662e-05


