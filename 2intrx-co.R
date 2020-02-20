#2-19-20
#EAP
#Second Interaction and carry over analysis

#load and process data
intrx.co = read.table(file = "2intrx-CO.csv", header = T, sep = ",")
str(intrx.co)
names(intrx.co)

#total interaction seconds
intrx.co$total.intrx = intrx.co$CSP + intrx.co$Cgroom + intrx.co$COAS + intrx.co$HSP + intrx.co$Hgroom + intrx.co$HOAS + intrx.co$NAS + intrx.co$Other + intrx.co$Ab + intrx.co$Tr + intrx.co$In 

#total carry over seconds
intrx.co$total.co = intrx.co$CSPCO + intrx.co$CgroomCO + intrx.co$COASCO + intrx.co$HSPCO + intrx.co$HgroomCO + intrx.co$HOASCO + intrx.co$NASCO + intrx.co$OtherCO + intrx.co$AbCO + intrx.co$TrCO + intrx.co$InCO 

#calculate proportions intrx
intrx.co$CSP.p = intrx.co$CSP/intrx.co$total.intrx
intrx.co$Cgroom.p = intrx.co$Cgroom/intrx.co$total.intrx
intrx.co$COAS.p = intrx.co$COAS/intrx.co$total.intrx
intrx.co$HSP.p = intrx.co$HSP/intrx.co$total.intrx
intrx.co$Hgroom.p = intrx.co$Hgroom/intrx.co$total.intrx
intrx.co$HOAS.p = intrx.co$HOAS/intrx.co$total.intrx
intrx.co$NAS.p = intrx.co$NAS/intrx.co$total.intrx
intrx.co$Other.p = intrx.co$Other/intrx.co$total.intrx
intrx.co$Ab.p = intrx.co$Ab/intrx.co$total.intrx
intrx.co$Tr.p = intrx.co$Tr/intrx.co$total.intrx
intrx.co$In.p = intrx.co$In/intrx.co$total.intrx



#calculate proportions carry over
intrx.co$CSPCO.p = intrx.co$CSPCO/intrx.co$total.co
intrx.co$CgroomCO.p = intrx.co$CgroomCO/intrx.co$total.co
intrx.co$COASCO.p = intrx.co$COASCO/intrx.co$total.co
intrx.co$HSPCO.p = intrx.co$HSPCO/intrx.co$total.co
intrx.co$HgroomCO.p = intrx.co$HgroomCO/intrx.co$total.co
intrx.co$HOASCO.p = intrx.co$HOASCO/intrx.co$total.co
intrx.co$NASCO.p = intrx.co$NASCO/intrx.co$total.co
intrx.co$OtherCO.p = intrx.co$OtherCO/intrx.co$total.co
intrx.co$AbCO.p = intrx.co$AbCO/intrx.co$total.co
intrx.co$TrCO.p = intrx.co$TrCO/intrx.co$total.co
intrx.co$InCO.p = intrx.co$InCO/intrx.co$total.co

#calculate difference of interaction and carry over
intrx.co$CSP.d = intrx.co$CSP.p - intrx.co$CSPCO.p
intrx.co$Cgroom.d = intrx.co$Cgroom.p - intrx.co$CgroomCO.p
intrx.co$COAS.d = intrx.co$COAS.p - intrx.co$COASCO.p
intrx.co$HSP.d = intrx.co$HSP.p - intrx.co$HSPCO.p
intrx.co$Hgroom.d = intrx.co$Hgroom.p - intrx.co$HgroomCO.p
intrx.co$HOAS.d = intrx.co$HOAS.p - intrx.co$HOASCO.p
intrx.co$NAS.d = intrx.co$NAS.p - intrx.co$NASCO.p
intrx.co$Other.d = intrx.co$Other.p - intrx.co$OtherCO.p
intrx.co$Ab.d = intrx.co$Ab.p - intrx.co$AbCO.p
intrx.co$Tr.d = intrx.co$Tr.p - intrx.co$TrCO.p
intrx.co$In.d = intrx.co$In.p - intrx.co$InCO.p




#condense dependent variables
y <- cbind(intrx.co$CSP.d, intrx.co$Cgroom.d, intrx.co$COAS.d, intrx.co$HSP.d, intrx.co$Hgroom.d, intrx.co$HOAS.d, intrx.co$NAS.d, intrx.co$Other.d, intrx.co$Ab.d, intrx.co$Tr.d, intrx.co$In.d)#combines dependent variables

#run manova
manova(y ~ Condition * Life + Chimp, data=intrx.co, na.action=na.omit)
M1 <- manova(y ~ Condition * Life + Chimp, data=intrx.co, na.action=na.omit)
summary(M1, tol=0)
summary.aov(M1)


#try a different way to analyze
t.test(intrx.co$HSP.p, intrx.co$HSPCO.p, paired=T) 
t.test(intrx.co$Hgroom.p, intrx.co$HgroomCO.p, paired=T) 
t.test(intrx.co$HOAS.p, intrx.co$HOASCO.p, paired=T)
t.test(intrx.co$CSP.p, intrx.co$CSPCO.p, paired=T)
t.test(intrx.co$Cgroom.p, intrx.co$Cgroom.p, paired=T)
t.test(intrx.co$COAS.p, intrx.co$COASCO.p, paired=T)
t.test(intrx.co$NAS.p, intrx.co$NASCO.p, paired=T) 
t.test(intrx.co$Other.p, intrx.co$OtherCO.p, paired=T) 
t.test(intrx.co$Ab.p, intrx.co$AbCO.p, paired=T) 
t.test(intrx.co$Tr.p, intrx.co$TrCO.p, paired=T) 
t.test(intrx.co$In.p, intrx.co$InCO.p, paired=T) #

#test of differences in behavior within the interaction session
aov(HSP ~ Condition * Life + Chimp, data=intrx.co, na.action=na.omit)
A1 <- aov(HSP ~ Condition * Life + Chimp, data=intrx.co, na.action=na.omit)
summary(A1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(A1)

aov(Hgroom ~ Condition * Life + Chimp, data=intrx.co, na.action=na.omit)
A2 <- aov(Hgroom ~ Condition * Life + Chimp, data=intrx.co, na.action=na.omit)
summary(A2,tol=0)
summary.aov(A2)

aov(HOAS ~ Condition * Life + Chimp, data=intrx.co, na.action=na.omit)
A3 <- aov(HOAS ~ Condition * Life + Chimp, data=intrx.co, na.action=na.omit)
summary(A3,tol=0)

aov(Other ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A4 <- aov(Other ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A4,tol=0)

aov(Ab ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A5 <- aov(Ab ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A5,tol=0)

aov(Tr ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A6 <- aov(Tr ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A6,tol=0)

aov(In ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A7 <- aov(In ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A7,tol=0)

