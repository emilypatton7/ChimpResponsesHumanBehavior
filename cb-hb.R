#5-Feb-2020
#EAP
#CB vs HB

#load and process data
cb.hb = read.table(file = "cb-hb.csv", header = T, sep = ",")
str(cb.hb)
names(cb.hb)


#total cb seconds
cb.hb$total.cb = cb.hb$CAfCB + cb.hb$HAfCB + cb.hb$NASCB + cb.hb$OtherCB + cb.hb$AbCB + cb.hb$TrCB + cb.hb$InCB

#total hb seconds
cb.hb$total.hb = cb.hb$CAfHB + cb.hb$HAfHB + cb.hb$NASHB + cb.hb$OtherHB + cb.hb$AbHB + cb.hb$TrHB + cb.hb$InHB

#calculate proportions cb
cb.hb$CAfCB.p = cb.hb$CAfCB/cb.hb$total.cb
cb.hb$HAfCB.p = cb.hb$HAfCB/cb.hb$total.cb
cb.hb$NASCB.p = cb.hb$NASCB/cb.hb$total.cb
cb.hb$OtherCB.p = cb.hb$OtherCB/cb.hb$total.cb
cb.hb$AbCB.p = cb.hb$AbCB/cb.hb$total.cb
cb.hb$TrCB.p = cb.hb$TrCB/cb.hb$total.cb
cb.hb$InCB.p = cb.hb$InCB/cb.hb$total.cb

#calculate proportions hb
cb.hb$CAfHB.p = cb.hb$CAfHB/cb.hb$total.hb
cb.hb$HAfHB.p = cb.hb$HAfHB/cb.hb$total.hb
cb.hb$NASHB.p = cb.hb$NASHB/cb.hb$total.hb
cb.hb$OtherHB.p = cb.hb$OtherHB/cb.hb$total.hb
cb.hb$AbHB.p = cb.hb$AbHB/cb.hb$total.hb
cb.hb$TrHB.p = cb.hb$TrHB/cb.hb$total.hb
cb.hb$InHB.p = cb.hb$InHB/cb.hb$total.hb



#run paired t-tests to find difference between interaction and matched control
#paired t-test of interaction v matched control for each behavior, p = 0.007
t.test(cb.hb$HAfCB.p, cb.hb$HAfHB.p, paired=T) 
t.test(cb.hb$CAfCB.p, cb.hb$CAfHB.p, paired=T) 
t.test(cb.hb$NASCB.p, cb.hb$NASHB.p, paired=T) 
t.test(cb.hb$OtherCB.p, cb.hb$OtherHB.p, paired=T) 
t.test(cb.hb$AbCB.p, cb.hb$AbHB.p, paired=T) 
t.test(cb.hb$TrCB.p, cb.hb$TrHB.p, paired=T)
t.test(cb.hb$InCB.p, cb.hb$InHB.p, paired=T) 

#test of differences in behavior within the interaction session
aov(HAf ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A1 <- aov(HAf ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A1, tol=0)#tol=0 overrides error code, overall test summary
summary.aov(A1)

aov(CAf ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A2 <- aov(CAf ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A2,tol=0)

aov(NAS ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A3 <- aov(NAS ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A3,tol=0)

aov(NAS ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A3 <- aov(NAS ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A3,tol=0)

aov(Ab ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A4 <- aov(Ab ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A4,tol=0)

aov(Tr ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A5 <- aov(Tr ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A5,tol=0)

aov(In ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
A6 <- aov(In ~ Condition * Life + Chimp, data=intrx.mc, na.action=na.omit)
summary(A6,tol=0)
