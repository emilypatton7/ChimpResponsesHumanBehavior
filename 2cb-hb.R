#19-Feb-2020
#EAP
#second CB vs HB

#load and process data
cb.hb = read.table(file = "2CB-HB.csv", header = T, sep = ",")
str(cb.hb)
names(cb.hb)


#total cb seconds
cb.hb$total.cb = cb.hb$CSPCB + cb.hb$CgroomCB + cb.hb$COASCB + cb.hb$HSPCB + cb.hb$HgroomCB + cb.hb$HOASCB + cb.hb$NASCB + cb.hb$OtherCB + cb.hb$AbCB + cb.hb$TrCB + cb.hb$InCB

#total hb seconds
cb.hb$total.hb = cb.hb$CSPHB + cb.hb$CgroomHB + cb.hb$COASHB + cb.hb$HSPHB + cb.hb$HgroomHB + cb.hb$HOASHB + cb.hb$NASHB + cb.hb$OtherHB + cb.hb$AbHB + cb.hb$TrHB + cb.hb$InHB

#calculate proportions cb
cb.hb$CSPCB.p = cb.hb$CSPCB/cb.hb$total.cb
cb.hb$CgroomCB.p = cb.hb$CgroomCB/cb.hb$total.cb
cb.hb$COASCB.p = cb.hb$COASCB/cb.hb$total.cb
cb.hb$HSPCB.p = cb.hb$HSPCB/cb.hb$total.cb
cb.hb$HgroomCB.p = cb.hb$HgroomCB/cb.hb$total.cb
cb.hb$HOASCB.p = cb.hb$HOASCB/cb.hb$total.cb
cb.hb$NASCB.p = cb.hb$NASCB/cb.hb$total.cb
cb.hb$OtherCB.p = cb.hb$OtherCB/cb.hb$total.cb
cb.hb$AbCB.p = cb.hb$AbCB/cb.hb$total.cb
cb.hb$TrCB.p = cb.hb$TrCB/cb.hb$total.cb
cb.hb$InCB.p = cb.hb$InCB/cb.hb$total.cb



#calculate proportions hb
cb.hb$CSPHB.p = cb.hb$CSPHB/cb.hb$total.hb
cb.hb$CgroomHB.p = cb.hb$CgroomHB/cb.hb$total.hb
cb.hb$COASHB.p = cb.hb$COASHB/cb.hb$total.hb
cb.hb$HSPHB.p = cb.hb$HSPHB/cb.hb$total.hb
cb.hb$HgroomHB.p = cb.hb$HgroomHB/cb.hb$total.hb
cb.hb$HOASHB.p = cb.hb$HOASHB/cb.hb$total.hb
cb.hb$NASHB.p = cb.hb$NASHB/cb.hb$total.hb
cb.hb$OtherHB.p = cb.hb$OtherHB/cb.hb$total.hb
cb.hb$AbHB.p = cb.hb$AbHB/cb.hb$total.hb
cb.hb$TrHB.p = cb.hb$TrHB/cb.hb$total.hb
cb.hb$InHB.p = cb.hb$InHB/cb.hb$total.hb


#run paired t-tests to find difference between interaction and matched control
#paired t-test of interaction v matched control for each behavior, p = 0.007
t.test(cb.hb$HSPCB.p, cb.hb$HSPHB.p, paired=T) 
t.test(cb.hb$HgroomCB.p, cb.hb$HgroomHB.p, paired=T) 
t.test(cb.hb$HOASCB.p, cb.hb$HOASHB.p, paired=T) 
t.test(cb.hb$CSPCB.p, cb.hb$CSPHB.p, paired=T) 
t.test(cb.hb$CgroomCB.p, cb.hb$CgroomHB.p, paired=T) 
t.test(cb.hb$COASCB.p, cb.hb$COASHB.p, paired=T) 
t.test(cb.hb$NASCB.p, cb.hb$NASHB.p, paired=T)
t.test(cb.hb$OtherCB.p, cb.hb$OtherHB.p, paired=T) 
t.test(cb.hb$AbCB.p, cb.hb$AbHB.p, paired=T) 
t.test(cb.hb$TrCB.p, cb.hb$TrHB.p, paired=T)
t.test(cb.hb$InCB.p, cb.hb$InHB.p, paired=T)


###test of differences in behavior within the interaction session in 2intrx-mc.R script
