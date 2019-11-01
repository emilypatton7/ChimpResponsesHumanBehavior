#1-Nov-19
#CPA and EAP
#Carry over and carry over matched control analysis

#load and process data
co.comc = read.table(file="CO.COMC.csv", header = T, sep = ",")#load table into R
str(co.comc)#returns structure of table

#total carry over seconds
co.comc$total.co = co.comc$CAfCO + co.comc$HAfCO + co.comc$NASCO + co.comc$OtherCO + co.comc$AbCO + co.comc$TrCO + co.comc$InCO

#total carry over matched control seconds
co.comc$total.comc = co.comc$CafCOMC + co.comc$HafCOMC + co.comc$NASCOMC + co.comc$OtherCOMC + co.comc$AbCOMC+ co.comc$TrCOMC + co.comc$InCOMC

#calculate proportions CO
co.comc$CAfCO.p = co.comc$CAfCO/co.comc$total.co
co.comc$HAfCO.p = co.comc$HAfCO/co.comc$total.co
co.comc$NASCO.p = co.comc$NASCO/co.comc$total.co
co.comc$OtherCO.p = co.comc$OtherCO/co.comc$total.co
co.comc$AbCO.p = co.comc$AbCO/co.comc$total.co
co.comc$TrCO.p = co.comc$TrCO/co.comc$total.co
co.comc$InCO.p = co.comc$InCO/co.comc$total.co

#calculate proportions COMC
co.comc$CAfCOMC.p = co.comc$CafCOMC/co.comc$total.comc
co.comc$HAfCOMC.p = co.comc$HafCOMC/co.comc$total.comc
co.comc$NASCOMC.p = co.comc$NASCOMC/co.comc$total.comc
co.comc$OtherCOMC.p = co.comc$OtherCOMC/co.comc$total.comc
co.comc$AbCOMC.p = co.comc$AbCOMC/co.comc$total.comc
co.comc$TrCOMC.p = co.comc$TrCOMC/co.comc$total.comc
co.comc$InCOMC.p = co.comc$InCOMC/co.comc$total.comc

#calculate difference of CO and COMC
co.comc$Caf.d = co.comc$CAfCO.p - co.comc$CAfCOMC.p
co.comc$Haf.d = co.comc$HAfCO.p - co.comc$HAfCOMC.p
co.comc$Nas.d = co.comc$NASCO.p - co.comc$NASCOMC.p
co.comc$Other.d = co.comc$OtherCO.p - co.comc$OtherCOMC.p
co.comc$Ab.d = co.comc$AbCO.p - co.comc$AbCOMC.p
co.comc$Tr.d = co.comc$TrCO.p - co.comc$TrCOMC.p
co.comc$In.d = co.comc$InCO.p - co.comc$InCOMC.p

#graphs
hist(co.comc$Caf.d)
