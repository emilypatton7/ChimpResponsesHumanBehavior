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
