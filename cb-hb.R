#5-Feb-2020
#EAP
#CB vs HB

#load and process data
cb.hb = read.table(file = "CB-HB.csv", header = T, sep = ",")
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
t.test(cb.hb$CAfCB.p, cb.hb$CAfHB.p, paired=T) 
t.test(cb.hb$HAfCB.p, cb.hb$HAfHB.p, paired=T) 
t.test(cb.hb$NASCB.p, cb.hb$NASHB.p, paired=T)
t.test(cb.hb$OtherCB.p, cb.hb$OtherHB.p, paired=T) 
t.test(cb.hb$AbCB.p, cb.hb$AbHB.p, paired=T) 
t.test(cb.hb$TrCB.p, cb.hb$TrHB.p, paired=T)
t.test(cb.hb$InCB.p, cb.hb$InHB.p, paired=T) 
