rm (list = ls(all=TRUE))
graphics.off()
install.packages("normtest")
install.packages("pastecs")
install.packages("sp")
install.packages("smooth")
install.packages("writexl")
library(sp)
library(pastecs)
library(normtest)
library(readxl)
library(writexl)
rawdata <- read_excel("C:/Users/islamr/OneDrive - University of Tasmania/Mardi Meetings/Meeting 22/DataSeries_returns/DataSets/rawreturnsdata.xlsx")
filterdata <- read_excel("C:/Users/islamr/OneDrive - University of Tasmania/Mardi Meetings/Meeting 22/DataSeries_returns/DataSets/filterdata.xlsx")
returns<-filterdata[,(1:35)]
install.packages("vars")
library(vars)
install.packages("frequencyConnectedness")
library(frequencyConnectedness)
install.packages("devtools")
library(devtools)
install_github("tomaskrehlik/frequencyConnectedness", tag = "dev") 
# Compute the VAR(2) estimate with constant and save results
est <- VAR(returns, p = 2, type = "const")
# Alternatively, you could use VECM
# est <- vec2var(ca.jo(exampleSim, ecdet = "trend", K = 2), r = 1)
# Compute traditional spillovers
sp1<-spilloverDY12(est, n.ahead = 10, no.corr = F)
write.csv(sp1, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/sp1.csv", row.names=F)
sp1overall<-overall(sp1)
write.csv(sp1overall, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/sp1overall.csv", row.names=F)
sp1to<-to(sp1)
write.csv(sp1to, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/sp1to.csv", row.names=F)
sp1from<-from(sp1)
write.csv(sp1from, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/sp1from.csv", row.names=F)
sp1net<-net(sp1)
write.csv(sp1net, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/sp1net.csv", row.names=F)
sp1pairwise<-pairwise(sp1)
write.csv(sp1pairwise, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/sp1pairwise.csv", row.names=F)
sp2<-spilloverDY12(est, n.ahead = 10, no.corr = T)
write.csv(sp2, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/sp2.csv", row.names=F)
sp2overall<-overall(sp2)
write.csv(sp2overall, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/sp2overall.csv", row.names=F)
sp2to<-to(sp2)
write.csv(sp2to, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/sp2to.csv", row.names=F)
sp2from<-from(sp2)
write.csv(sp2from, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/sp2from.csv", row.names=F)
sp2net<-net(sp2)
write.csv(sp2net, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/sp2net.csv", row.names=F)
sp2pairwise<-pairwise(sp2)
write.csv(sp2pairwise, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/sp2pairwise.csv", row.names=F)
#write.csv(Dates_converted_df, "C:/Users/Mihan/Dropbox/DataCamp/Dates_converted_df.csv", row.names=F)
# Get the frequency connectedness on partition (pi,pi/4), (pi/4,0), roughly
# corresponding to movements of 1 to 4 days and 4 to longer.
bounds <- c(pi+0.00001, pi/4, 0)
spilloverBK12A<- spilloverBK12(est, n.ahead = 100, no.corr = F, partition = bounds)
write.csv(spilloverBK12A, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12A.csv", row.names=F)
spilloverBK12Aoverall<-overall(spilloverBK12A)
write.csv(spilloverBK12Aoverall, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12Aoverall.csv", row.names=F)
spilloverBK12Ato<-to(spilloverBK12A)
write.csv(spilloverBK12Ato, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12Ato.csv", row.names=F)
spilloverBK12Afrom<-from(spilloverBK12A)
write.csv(spilloverBK12Afrom, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12Afrom.csv", row.names=F)
spilloverBK12Anet<-net(spilloverBK12A)
write.csv(spilloverBK12Anet, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12Anet.csv", row.names=F)
spilloverBK12Apairwise<-pairwise(spilloverBK12A)
write.csv(spilloverBK12Apairwise, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12Apairwise.csv", row.names=F)

spilloverBK12B<-spilloverBK12(est, n.ahead = 100, no.corr = T, partition = bounds)
write.csv(spilloverBK12B, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12B.csv", row.names=F)
spilloverBK12Boverall<-overall(spilloverBK12B)
write.csv(spilloverBK12Boverall, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12Boverall.csv", row.names=F)
spilloverBK12Bto<-to(spilloverBK12B)
write.csv(spilloverBK12Bto, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12Bto.csv", row.names=F)
spilloverBK12Bfrom<-from(spilloverBK12B)
write.csv(spilloverBK12Bfrom, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12Bfrom.csv", row.names=F)
spilloverBK12Bnet<-net(spilloverBK12B)
write.csv(spilloverBK12Bnet, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12Bnet.csv", row.names=F)
spilloverBK12Bpairwise<-pairwise(spilloverBK12B)
write.csv(spilloverBK12Bpairwise, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12Bpairwise.csv", row.names=F)

# Get the rolling window estimates
params_est = list(p = 2, type = "const")
spDY12rolling <- spilloverRollingDY12(returns, n.ahead = 10, no.corr = F, "VAR", params_est = params_est, window = 100)
write.csv(spDY12rolling, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spDY12rolling.csv", row.names=F)
spDY12rollingoverall<-overall(spDY12rolling)
write.csv(spDY12rollingoverall, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spDY12rollingoverall.csv", row.names=F)

spDY12rollingto<-to(spDY12rolling)
write.csv(spDY12rollingto, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spDY12rollingto.csv", row.names=F)

spDY12rollingfrom<-from(spDY12rolling)
write.csv(spDY12rollingfrom, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spDY12rollingfrom.csv", row.names=F)

spDY12rollingpairwise<-pairwise(spDY12rolling)
write.csv(spDY12rollingpairwise, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spDY12rollingpairwise.csv", row.names=F)

spDY12rollingnet<-net(spDY12rolling)
write.csv(spDY12rollingnet, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spDY12rollingnet.csv", row.names=F)

# BK12 estimates
spilloverBK12rolling<- spilloverRollingBK12(returns, n.ahead = 100, no.corr = F, partition = bounds, "VAR", params_est = params_est, window=100, cluster = NULL)
write.csv(spilloverBK12rolling, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12rolling.csv", row.names=F)

spilloverBK12rollingto<-to(spilloverBK12rolling)
write.csv(spilloverBK12rollingto, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12rollingto.csv", row.names=F)

spilloverBK12rollingfrom<-from(spilloverBK12rolling)
write.csv(spilloverBK12rollingfrom, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12rollingfrom.csv", row.names=F)

spilloverBK12rollingnet<-net(spilloverBK12rolling)
write.csv(spilloverBK12rollingnet, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12rollingnet.csv", row.names=F)

spilloverBK12rollingpairwise<-pairwise(spilloverBK12rolling)
write.csv(spilloverBK12rollingpairwise, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12rollingpairwise.csv", row.names=F)

spilloverBK12rollingoverall<- overall(spilloverBK12rolling)
write.csv(spilloverBK12rollingoverall, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12rollingoverall.csv", row.names=F)

save(sp1,sp2,spilloverBK12A,spilloverBK12B,spDY12rolling,spilloverBK12rolling,  file = "importantVDmatrices.RData")

# Now save the spillover tables only rolling
spDY12rolling <- as.matrix(spilloverRollingDY12(returns, n.ahead = 10, no.corr = F, "VAR", params_est = params_est, window = 100))
write.csv(spDY12rolling, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spDY12rolling.csv", row.names=F)

spilloverBK12rolling<- as.matrix(spilloverRollingBK12(returns, n.ahead = 100, no.corr = F, partition = bounds, "VAR", params_est = params_est, window=100, cluster = NULL))
write.csv(spilloverBK12rolling, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12rolling.csv", row.names=F)


sp1<-as.matrix(spilloverDY12(est, n.ahead = 10, no.corr = F))
write.csv(sp1, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverDY2012withownshock.csv", row.names=F)
sp2<-as.matrix(spilloverDY12(est, n.ahead = 10, no.corr = T))
write.csv(sp2, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverDY2012withNOownshock.csv", row.names=F)
spilloverBK12A<- as.matrix(spilloverBK12(est, n.ahead = 100, no.corr = F, partition = bounds))
write.csv(spilloverBK12A, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12withownshock.csv", row.names=F)
spilloverBK12B<-as.matrix(spilloverBK12(est, n.ahead = 100, no.corr = T, partition = bounds))
write.csv(spilloverBK12B, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12withNOownshock.csv", row.names=F)
# Now do the analysis setting diagonal elements to zero , so no own shock
params_est = list(p = 2, type = "const")
spDY12rollingnoownshock <- spilloverRollingDY12(returns, n.ahead = 10, no.corr = T, "VAR", params_est = params_est, window = 100)
write.csv(spDY12rollingnoownshock, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spDY12rollingnoownshock.csv", row.names=F)

spDY12rollingoverallnoownshock<-overall(spDY12rollingnoownshock)
write.csv(spDY12rollingoverallnoownshock, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spDY12rollingoverallnoownshock.csv", row.names=F)

spDY12rollingtonoownshock<-to(spDY12rollingnoownshock)
write.csv(spDY12rollingtonoownshock, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spDY12rollingtonoownshock.csv", row.names=F)

spDY12rollingfromnoownshock<-from(spDY12rollingnoownshock)
write.csv(spDY12rollingfromnoownshock, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spDY12rollingfromnoownshock.csv", row.names=F)

spDY12rollingpairwisenoownshock<-pairwise(spDY12rollingnoownshock)
write.csv(spDY12rollingpairwisenoownshock, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spDY12rollingpairwisenoownshock.csv", row.names=F)

spDY12rollingnetnoownshock<-net(spDY12rollingnoownshock)
write.csv(spDY12rollingnetnoownshock, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spDY12rollingnetnoownshock.csv", row.names=F)

spDY12rollingnoownshock <- as.matrix(spilloverRollingDY12(returns, n.ahead = 10, no.corr = T, "VAR", params_est = params_est, window = 100))
write.csv(spDY12rollingnoownshock, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spDY12rollingnoownshock.csv", row.names=F)

# BK12 estimates with no own shocks
spilloverBK12rollingnoownshocks<- spilloverRollingBK12(returns, n.ahead = 100, no.corr = T, partition = bounds, "VAR", params_est = params_est, window=100, cluster = NULL)
write.csv(spilloverBK12rollingnoownshocks, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12rollingnoownshocks.csv", row.names=F)

spilloverBK12rollingtonoownshocks<-to(spilloverBK12rollingnoownshocks)
write.csv(spilloverBK12rollingtonoownshocks, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12rollingtonoownshocks.csv", row.names=F)

spilloverBK12rollingfromnoownshocks<-from(spilloverBK12rollingnoownshocks)
write.csv(spilloverBK12rollingfromnoownshocks, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12rollingfromnoownshocks.csv", row.names=F)

spilloverBK12rollingnetnoownshocks<-net(spilloverBK12rollingnoownshocks)
write.csv(spilloverBK12rollingnetnoownshocks, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12rollingnetnoownshocks.csv", row.names=F)

spilloverBK12rollingpairwisenoownshocks<-pairwise(spilloverBK12rollingnoownshocks)
write.csv(spilloverBK12rollingpairwisenoownshocks, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12rollingpairwisenoownshocks.csv", row.names=F)

spilloverBK12rollingoverallnoownshocks<- overall(spilloverBK12rollingnoownshocks)
write.csv(spilloverBK12rollingoverallnoownshocks, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12rollingoverallnoownshocks.csv", row.names=F)

spilloverBK12rollingnoownshocks<- as.matrix(spilloverRollingBK12(returns, n.ahead = 100, no.corr = T, partition = bounds, "VAR", params_est = params_est, window=100, cluster = NULL))
write.csv(spilloverBK12rollingnoownshocks, "C:/Users/islamr/OneDrive - University of Tasmania/DataCamp/R/Expeimenting_with_frequencyconnectedness/spilloverBK12rollingnoownshocks.csv", row.names=F)

save.image("Allestimatessofar.RData")