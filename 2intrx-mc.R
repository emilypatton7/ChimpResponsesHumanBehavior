#19-Feb-2020
#EAP
#Second Interaction and matched control analysis


#load and process data
intrx.mc = read.table(file = "2intrx-mc.csv", header = T, sep = ",")
str(intrx.mc)
names(intrx.mc)

#total interaction seconds
intrx.mc$total.intrx = intrx.mc$CSP + intrx.mc$Cgroom + intrx.mc$COAS + intrx.mc$HSP + intrx.mc$Hgroom + intrx.mc$HOAS + intrx.mc$Other + intrx.mc$Ab + intrx.mc$Tr + intrx.mc$In

#total matched control seconds
intrx.mc$total.mc = intrx.mc$CSPMC + intrx.mc$CgroomMC + intrx.mc$COASMC + intrx.mc$HSPMC + intrx.mc$HgroomMC + intrx.mc$HOASMC + intrx.mc$OtherMC + intrx.mc$AbMC + intrx.mc$TrMC + intrx.mc$InMC

#calculate proportions interaction
intrx.mc$CSP.p = intrx.mc$CSP/intrx.mc$total.intrx
intrx.mc$Cgroom.p = intrx.mc$Cgroom/intrx.mc$total.intrx
intrx.mc$COAS.p = intrx.mc$COAS/intrx.mc$total.intrx
intrx.mc$HSP.p = intrx.mc$HSP/intrx.mc$total.intrx
intrx.mc$Hgroom.p = intrx.mc$Hgroom/intrx.mc$total.intrx
intrx.mc$HOAS.p = intrx.mc$HOAS/intrx.mc$total.intrx
intrx.mc$NAS.p = intrx.mc$NAS/intrx.mc$total.intrx
intrx.mc$Other.p = intrx.mc$Other/intrx.mc$total.intrx
intrx.mc$Ab.p = intrx.mc$Ab/intrx.mc$total.intrx
intrx.mc$Tr.p = intrx.mc$Tr/intrx.mc$total.intrx
intrx.mc$In.p = intrx.mc$In/intrx.mc$total.intrx

#calculate proportions matched control
intrx.mc$CSPMC.p = intrx.mc$CSPMC/intrx.mc$total.mc
intrx.mc$CgroomMC.p = intrx.mc$CgroomMC/intrx.mc$total.mc
intrx.mc$COASMC.p = intrx.mc$COASMC/intrx.mc$total.mc
intrx.mc$HSPMC.p = intrx.mc$HSPMC/intrx.mc$total.mc
intrx.mc$HgroomMC.p = intrx.mc$HgroomMC/intrx.mc$total.mc
intrx.mc$HOASMC.p = intrx.mc$HOASMC/intrx.mc$total.mc
intrx.mc$NASMC.p = intrx.mc$NASMC/intrx.mc$total.mc
intrx.mc$OtherMC.p = intrx.mc$OtherMC/intrx.mc$total.mc
intrx.mc$AbMC.p = intrx.mc$AbMC/intrx.mc$total.mc
intrx.mc$TrMC.p = intrx.mc$TrMC/intrx.mc$total.mc
intrx.mc$InMC.p = intrx.mc$InMC/intrx.mc$total.mc

#calculate difference of interaction and matched control
intrx.mc$CSP.d = intrx.mc$CSP.p - intrx.mc$CSPMC.p
intrx.mc$Cgroom.d = intrx.mc$Cgroom.p - intrx.mc$CgroomMC.p
intrx.mc$COAS.d = intrx.mc$COAS.p - intrx.mc$COAS.p
intrx.mc$HSP.d = intrx.mc$HSP.p - intrx.mc$HSPMC.p
intrx.mc$Hgroom.d = intrx.mc$Hgroom.p - intrx.mc$HgroomMC.p
intrx.mc$HOAS.d = intrx.mc$HOAS.p - intrx.mc$HOASMC.p
intrx.mc$NAS.d = intrx.mc$NAS.p - intrx.mc$NASMC.p
intrx.mc$Other.d = intrx.mc$Other.p - intrx.mc$OtherMC.p
intrx.mc$Ab.d = intrx.mc$Ab.p - intrx.mc$AbMC.p
intrx.mc$Tr.d = intrx.mc$Tr.p - intrx.mc$TrMC.p
intrx.mc$In.d = intrx.mc$In.p - intrx.mc$InMC.p



#condense dependent variables as proportional differences 
y <- cbind(intrx.mc$CSP.d, intrx.mc$Cgroom.d, intrx.mc$COAS.d, intrx.mc$HSP.d, intrx.mc$Hgroom.d, intrx.mc$HOAS.d, intrx.mc$NAS.d, intrx.mc$Other.d, intrx.mc$Ab.d, intrx.mc$Tr.d, intrx.mc$In.d)#combines dependent variables


#run manova
manova(y ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
M1 <- manova(y ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)


#run paired t-tests to find difference between interaction and matched control
#paired t-test of interaction v matched control for each behavior, p = 0.007t.test(intrx.mc$HSP.p, intrx.mc$HSPMC.p, paired=T) 
t.test(intrx.mc$Hgroom.p, intrx.mc$HgroomMC.p, paired=T) 
t.test(intrx.mc$HOAS.p, intrx.mc$HOASMC.p, paired=T) 
t.test(intrx.mc$CSP.p, intrx.mc$CSPMC.p, paired=T) 
t.test(intrx.mc$Cgroom.p, intrx.mc$CgroomMC.p, paired=T) 
t.test(intrx.mc$COAS.p, intrx.mc$COAS.p, paired=T) 
t.test(intrx.mc$NAS.p, intrx.mc$NASMC.p, paired=T) 
t.test(intrx.mc$Other.p, intrx.mc$OtherMC.p, paired=T) 
t.test(intrx.mc$Ab.p, intrx.mc$AbMC.p, paired=T) 
t.test(intrx.mc$Tr.p, intrx.mc$TrMC.p, paired=T) 
t.test(intrx.mc$In.p, intrx.mc$InMC.p, paired=T) 


#test of differences in behavior within the interaction session
aov(CSP ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A1 <- aov(CSP ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(A1)

aov(Cgroom ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A2 <- aov(Cgroom ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A2,tol=0)

aov(COAS ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A3 <- aov(COAS ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A3,tol=0)

aov(HSP ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A4 <- aov(HSP ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A4,tol=0)

aov(Hgroom ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A5 <- aov(Hgroom ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A5,tol=0)

aov(HOAS ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A6 <- aov(HOAS ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A6,tol=0)

aov(NAS ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A7 <- aov(NAS ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A7,tol=0)

aov(Other ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A8 <- aov(Other ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A8,tol=0)

aov(Ab ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A9 <- aov(Ab ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A9,tol=0)

aov(Tr ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A10 <- aov(Tr ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A10,tol=0)

aov(In ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A11 <- aov(In ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A11,tol=0)
