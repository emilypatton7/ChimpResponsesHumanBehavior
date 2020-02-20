#19-Feb-20
#EAP
#Second Carry over and carry over matched control analysis

#load and process data
co.comc = read.table(file="2CO-COMC.csv", header = T, sep = ",")#load table into R
str(co.comc)#returns structure of table
names(co.comc)

#total carry over seconds
co.comc$total.co = co.comc$CSPCO + co.comc$CgroomCO + co.comc$COASCO + co.comc$HSPCO + co.comc$HgroomCO + co.comc$HOASCO + co.comc$NASCO + co.comc$OtherCO + co.comc$AbCO + co.comc$TrCO + co.comc$InCO

#total carry over matched control seconds
co.comc$total.comc = co.comc$CSPCOMC + co.comc$CgroomCOMC + co.comc$COASCOMC + co.comc$HSPCOMC + co.comc$HgroomCOMC + co.comc$HOASCOMC + co.comc$NASCOMC + co.comc$OtherCOMC + co.comc$AbCOMC+ co.comc$TrCOMC + co.comc$InCOMC

#calculate proportions CO
co.comc$CSPCO.p = co.comc$CSPCO/co.comc$total.co
co.comc$CgroomCO.p = co.comc$CgroomCO/co.comc$total.co
co.comc$COASCO.p = co.comc$COASCO/co.comc$total.co
co.comc$HSPCO.p = co.comc$HSPCO/co.comc$total.co
co.comc$HgroomCO.p = co.comc$HgroomCO/co.comc$total.co
co.comc$HOASCO.p = co.comc$HOASCO/co.comc$total.co
co.comc$NASCO.p = co.comc$NASCO/co.comc$total.co
co.comc$OtherCO.p = co.comc$OtherCO/co.comc$total.co
co.comc$AbCO.p = co.comc$AbCO/co.comc$total.co
co.comc$TrCO.p = co.comc$TrCO/co.comc$total.co
co.comc$InCO.p = co.comc$InCO/co.comc$total.co

#calculate proportions COMC
co.comc$CSPCOMC.p = co.comc$CSPCOMC/co.comc$total.co
co.comc$CgroomCOMC.p = co.comc$CgroomCOMC/co.comc$total.co
co.comc$COASCOMC.p = co.comc$COASCOMC/co.comc$total.co
co.comc$HSPCOMC.p = co.comc$HSPCOMC/co.comc$total.co
co.comc$HgroomCOMC.p = co.comc$HgroomCOMC/co.comc$total.co
co.comc$HOASCOMC.p = co.comc$HOASCOMC/co.comc$total.co
co.comc$NASCOMC.p = co.comc$NASCOMC/co.comc$total.comc
co.comc$OtherCOMC.p = co.comc$OtherCOMC/co.comc$total.comc
co.comc$AbCOMC.p = co.comc$AbCOMC/co.comc$total.comc
co.comc$TrCOMC.p = co.comc$TrCOMC/co.comc$total.comc
co.comc$InCOMC.p = co.comc$InCOMC/co.comc$total.comc

#calculate difference of CO and COMC
co.comc$CSP.d = co.comc$CSPCO.p - co.comc$CSPCOMC.p
co.comc$Cgroom.d = co.comc$CgroomCO.p - co.comc$CgroomCOMC.p
co.comc$COAS.d = co.comc$COASCO.p - co.comc$COASCOMC.p
co.comc$HSP.d = co.comc$HSPCO.p - co.comc$HSPCOMC.p
co.comc$Hgroom.d = co.comc$HgroomCO.p - co.comc$HgroomCOMC.p
co.comc$HOAS.d = co.comc$HOASCO.p - co.comc$HOASCOMC.p
co.comc$NAS.d = co.comc$NASCO.p - co.comc$NASCOMC.p
co.comc$Other.d = co.comc$OtherCO.p - co.comc$OtherCOMC.p
co.comc$Ab.d = co.comc$AbCO.p - co.comc$AbCOMC.p
co.comc$Tr.d = co.comc$TrCO.p - co.comc$TrCOMC.p
co.comc$In.d = co.comc$InCO.p - co.comc$InCOMC.p


#repeated measures approach with MANOVA
#condense dependent variables
y <- cbind(co.comc$CSP.d, co.comc$Cgroom.d, co.comc$COAS.d, co.comc$HSP.d, co.comc$Hgroom.d, co.comc$HOAS.d, co.comc$NAS.d, co.comc$Other.d, co.comc$Ab.d, co.comc$Tr.d, co.comc$In.d)#combines dependent variables


#run manova
manova(y ~ Condition * Life + Chimp, data=co.comc, na.action=na.omit)
M1 <- manova(y ~ Condition * Life + Chimp, data=co.comc, na.action=na.omit)
summary(M1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(M1)

#try a different way to analyze, paired t-test of interaction v matched control for each behavior, p = 0.007
t.test(co.comc$HSPCO.p, co.comc$HSPCOMC.p, paired=T) 
t.test(co.comc$HgroomCO.p, co.comc$HgroomCOMC.p, paired=T) 
t.test(co.comc$HOASCO.p, co.comc$HOASCOMC.p, paired=T)
t.test(co.comc$CSPCO.p, co.comc$CSPCOMC.p, paired=T)
t.test(co.comc$CgroomCO.p, co.comc$CgroomCOMC.p, paired=T)
t.test(co.comc$COASCO.p, co.comc$COASCOMC.p, paired=T)
t.test(co.comc$NASCO.p, co.comc$NASCOMC.p, paired=T) 
t.test(co.comc$OtherCO.p, co.comc$OtherCOMC.p, paired=T) 
t.test(co.comc$AbCO.p, co.comc$AbCOMC.p, paired=T) 
t.test(co.comc$TrCO.p, co.comc$TrCOMC.p, paired=T) 
t.test(co.comc$InCO.p, co.comc$InCOMC.p, paired=T) 


