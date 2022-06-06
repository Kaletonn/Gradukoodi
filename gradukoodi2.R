#tuodaan kirjastot ja alustetaan wd
library(urca)
library(forecast)
library(tidyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(tidyverse)
library(timetk)
library(xts)
library(dplyr)
library(tseries)
library(vars)
library(mFilter)
library(cowplot)

#inflaatiodata kuntoon

inf1 <- read.csv("C:/Users/omistaja/Desktop/Koulu/Gradu/Inflatio_EU_1980_2020.csv", head = TRUE)
inf1[1:3] <- list(NULL)
inf1 <- t(inf1)
inf1 <- subset(inf1, select = c(1:27))
maat <- inf1[1,]
inf1 <- inf1[-1,]
inf1 <- inf1[c(16:41),] 

inf1 <- matrix(as.numeric(inf1),    # Convert to numeric matrix
                  ncol = ncol(inf1))
inf1 <- as.data.frame((inf1))
colnames(inf1) <- maat
inf1 <- cbind(vuosi = 1995:2020, inf1)
rownames(inf1) <- inf1[,1]
inf1 <- inf1[,-1]


inf1 <- ts(inf1,start = c(1995.12), frequency = 1)
inf1 <- inf1[,c(1,6,8,9,11,13,15,18,25,26,27)]
inf1 <- inf1[,c(1,2,5,10,3,4,6,7,8,9,11)]
glimpse(inf1)


euroinf <- inf1[,c(1:27)]
euroinf <- rowMeans(euroinf,na.rm = T)
euroinf <- ts(euroinf,start = c(1995.12), frequency = 1)
plot(EUinf, main= "EU-inflaatio 1995 - 2020")

EUinf <- read_excel("EUinf.xlsx")
EUinf <- EUinf[c(1:26),]
EUinf <- EUinf[,-1] 
EUinf <- ts(EUinf,start = c(1995.12), frequency = 1)

inf <- cbind(EUinf,euroinf)
plot(inf)
#Inflaatiodata 1995-2020 ok


  
  
#velka/bkt data siivous

vbkt <- read.csv("C:/Users/omistaja/Desktop/Koulu/Gradu/Valtiovelka_gdb_9520.csv", head = TRUE)
vbkt <- vbkt[,c(1,6,7)]
vbkt[,1] <- as.factor(vbkt[,1])

vbkt <- spread(vbkt,TIME,Value)
vbkt <- t(vbkt)
maat <- vbkt[1,]
vbkt <- vbkt[-1,]

vbkt <- matrix(as.numeric(vbkt),    # Convert to numeric matrix
               ncol = ncol(vbkt))
vbkt <- as.data.frame((vbkt))
colnames(vbkt) <- maat
vbkt <- vbkt[,c(1,4,5,7,8,9,12,14,16,23,24)]
dvbkt <- tail(vbkt, -1) - head(vbkt, -1)
vbkt <- ts(vbkt,start = c(1995.12), frequency = 1)
dvbkt <- ts(dvbkt,start = c(1996.12), frequency = 1)


#Valtion ylijäämä datan siivous

yli <- read.csv("C:/Users/omistaja/Desktop/Koulu/Gradu/CAD_data.csv", head = TRUE)
yli <- yli[,c(1,6,7)]
yli[,2] <- as.factor(yli[,2])
yli <- spread(yli,TIME,Value)

yli <- t(yli)
maat <- yli[1,]
yli <- yli[-1,]
yli <- matrix(as.numeric(yli),    # Convert to numeric matrix
               ncol = ncol(yli))
yli <- as.data.frame((yli))
colnames(yli) <- maat
yli <- yli[,c(1,4,5,8,9,10,13,15,17,23,24)]
dyli <- tail(yli, -1) - head(yli, -1)
yli <- ts(yli,start = c(1995.12), frequency = 1)
dyli <- ts(dyli,start = c(1996.12), frequency = 1)


maat <- colnames(dyli)


#Kuvaajien piirto

plot(inf1[,1], type = "l", col = 1, lwd = 2, main = "Inflaatio 1995 - 2020",xlab = "Vuosi",
     ylab = "Inflaatio", ylim = c(-0.5,10.5),)
for (i in 2:11) {
  lines(inf1[,i], type = "l", lty = i ,lwd = 2, col = i)
}
abline(h=0, lty = 2)
legend("topright", inset = 0,12, legend=c("Itävalta", "Tsekki", "Saksa","Espanja","Viro",
                       "Suomi", "Unkari","Italia", "Luxemburg",
                       "Slovenia", "Ruotsi"), col=c(1:11), lty= c(1:11), lwd =2, cex=0.7)


plot(vbkt[,1], type = "l", col = 1, lwd = 2, main = "Valtion velka/BKT suhde 1995 - 2020",xlab = "Vuosi",
     ylab = "Velka/BKT", ylim = c(0,170))
for (i in 2:11) {
  lines(vbkt[,i], type = "l", lty = i, lwd = 2, col = i)
}
legend("topleft", inset = 0,12, legend=c("Itävalta", "Tsekki", "Saksa","Espanja","Viro",
                                          "Suomi", "Unkari","Italia", "Luxemburg",
                                          "Slovenia", "Ruotsi"), col=c(1:11), lty= c(1:11), lwd =2, cex=0.43)


plot(yli[,1], type = "l", main = "Valtion ylijäämä 1995 - 2020",xlab = "Vuosi",
     ylab = "Ylijäämä prosenttina bruttokansantuotteesta", col = 1, lwd = 2, ylim = c(-15,13))
for (i in 2:11) {
  lines(yli[,i], type = "l", lty = i, lwd = 2, col = i)
}
abline(h=0, lty = 2)
legend("bottomright", inset = 0,12, legend=c("Itävalta", "Tsekki", "Saksa","Espanja","Viro",
                                          "Suomi", "Unkari","Italia", "Luxemburg",
                                          "Slovenia", "Ruotsi"), col=c(1:11), lty= c(1:11), lwd =2, cex=0.7)



plot(dyli[,1], type = "l", main = "Valtion ylijäämän differentiaalit 1996 - 2020",xlab = "Vuosi",
     ylab = "Ylijäämä prosenttina bruttokansantuotteesta", col = 1, lwd = 2, ylim = c(-7,12))
for (i in 2:11) {
  lines(dyli[,i], type = "l", lty = i, lwd = 2, col = i)
}
abline(h=0, lty = 2)
legend("topright", inset = 0,12, legend=c("Itävalta", "Tsekki", "Saksa","Espanja","Viro",
                                             "Suomi", "Unkari","Italia", "Luxemburg",
                                             "Slovenia", "Ruotsi"), col=c(1:11), lty= c(1:11), lwd =2, cex=0.7)




plot(dvbkt[,1], type = "l", main = "Valtion velka/bkt differentiaalit 1996 - 2020",xlab = "Vuosi",
     ylab = "Ylijäämä prosenttina bruttokansantuotteesta", col = 1, lwd = 2, ylim = c(-11,20))
for (i in 2:11) {
  lines(dvbkt[,i], type = "l", lty = i, lwd = 2, col = i)
}
abline(h=0, lty = 2)
legend("topleft", inset = 0,12, legend=c("Itävalta", "Tsekki", "Saksa","Espanja","Viro",
                                             "Suomi", "Unkari","Italia", "Luxemburg",
                                             "Slovenia", "Ruotsi"), col=c(1:11), lty= c(1:11), lwd =2, cex=0.7)

maat <- colnames(dyli)


# Stationariteetti ja yksikköjuuritestaus


# 1. Phillips-Perron

ppdvbkt <- pp.test(dvbkt[c(1:24),1], alternative = "stationary", type = "Z(alpha)")
for (i in 2:11) {
  ppdvbkt <- cbind(ppdvbkt, pp.test(dvbkt[c(1:24),i], alternative = "stationary", type = "Z(alpha)"))
}
colnames(ppdvbkt) <- maat

#pp.test(dvbkt[c(1:24),2], alternative = "stationary", type = "Z(t_alpha)")

ppdyli <- pp.test(dyli[c(1:24),1], alternative = "stationary", type = "Z(alpha)")
for (i in 2:11) {
  ppdyli <- cbind(ppdyli, pp.test(dyli[c(1:24),i], alternative = "stationary", type = "Z(alpha)"))
}
colnames(ppdyli) <- maat


ppinf <- pp.test(inf1[c(2:25),1], alternative = "stationary", type = "Z(alpha)")
for (i in 2:11) {
  ppinf <- cbind(ppinf, pp.test(inf1[c(2:25),i], alternative = "stationary", type = "Z(alpha)"))
}
colnames(ppinf) <- maat


pp.test(vbkt[,2], alternative = "stationary", type = "Z(t_alpha)")
pp.test(vbkt[,2], alternative = "stationary", type = "Z(alpha)")

pp.test(vbkt[,3], alternative = "stationary", type = "Z(t_alpha)")
pp.test(vbkt[,3], alternative = "stationary", type = "Z(alpha)")

pp.test(vbkt[,4], alternative = "stationary", type = "Z(t_alpha)")
pp.test(vbkt[,4], alternative = "stationary", type = "Z(alpha)")

pp.test(vbkt[,5], alternative = "stationary", type = "Z(t_alpha)")
pp.test(vbkt[,5], alternative = "stationary", type = "Z(alpha)")

pp.test(vbkt[,6], alternative = "stationary", type = "Z(t_alpha)")
pp.test(vbkt[,6], alternative = "stationary", type = "Z(alpha)")

pp.test(vbkt[,7], alternative = "stationary", type = "Z(t_alpha)")
pp.test(vbkt[,7], alternative = "stationary", type = "Z(alpha)")

pp.test(vbkt[,8], alternative = "stationary", type = "Z(t_alpha)")
pp.test(vbkt[,8], alternative = "stationary", type = "Z(alpha)")

pp.test(vbkt[,9], alternative = "stationary", type = "Z(t_alpha)")
pp.test(vbkt[,9], alternative = "stationary", type = "Z(alpha)")

pp.test(vbkt[,10], alternative = "stationary", type = "Z(t_alpha)")
pp.test(vbkt[,10], alternative = "stationary", type = "Z(alpha)")


#------------------------------------------------------------------
# ADF

adfvbkt <- adf.test(vbkt[c(1:24),1], alternative = "stationary", k = 1)
for (i in 2:11) {
  adfvbkt <- cbind(adfdvbkt, adf.test(dvbkt[c(1:24),i], alternative = "stationary", k = 1))
}
colnames(adfvbkt) <- maat


adfyli <- adf.test(yli[c(1:24),1], alternative = "stationary", k = 1)
for (i in 2:11) {
  adfyli <- cbind(adfdyli, adf.test(dyli[c(1:24),i], alternative = "stationary", k = 1))
}
colnames(adfyli) <- maat


adfinf <- adf.test(inf1[c(2:25),1], alternative = "stationary", k = 1)
for (i in 2:11) {
  adfinf <- cbind(adfinf, adf.test(inf1[c(2:25),i], alternative = "stationary", k = 1))
}
colnames(adfinf) <- maat


#-----------------------------------------------------------------------------


# Nopea lineaari regressio

#ifna <- inf1[,1]
#ifna <- ifna [-1]
#vbkta <- dvbkt[,1]
#ylia <- dyli[,1]

#glimpse(ylia)

#OLS1 <- lm(ifna ~  vbkta + ylia)
#summary(OLS1)

#-----------------------------------------------------------------------------


#LAg selection ACF ja PACF

acf(inf1[,1])       #MA
pacf(inf1[,1])       #AR

acf(vbkt[,1])
pacf(vbkt[,1])

acf(yli[,1])
pacf(yli[,1])

#AUT2 <- cbind(inf1[2:26,1],dvbkt[,1],dyli[,1])
#lagselect <- VARselect(AUT2,lag.max = 10, type = "const")
#lagselect$selection


AUT <- cbind(inf1[,1],vbkt[,1],yli[,1])
colnames(AUT) <- cbind("inf","vbkt","yli")
lagselect <- VARselect(AUT,lag.max = 10, type = "const")
lagselect$selection 
# 5
AUT2 <- cbind(vbkt[,1],yli[,1])
colnames(AUT2) <- cbind("vbkt","yli")
lagselect <- VARselect(AUT2,lag.max = 10, type = "const")
lagselect$selection 
# 8


CZE <- cbind(inf1[,2],vbkt[,2],yli[,2])
lagselect <- VARselect(CZE,lag.max = 10, type = "const")
lagselect$selection 
#5 lagia
CZE2 <- cbind(vbkt[,2],yli[,2])
colnames(CZE2) <- cbind("vbkt","yli")
lagselect <- VARselect(CZE2,lag.max = 10, type = "const")
lagselect$selection 
#7 lagia

DEU <- cbind(inf1[,3],vbkt[,3],yli[,3])
lagselect <- VARselect(DEU,lag.max = 10, type = "const")
lagselect$selection
#5
DEU2 <- cbind(vbkt[,2],yli[,2])
colnames(DEU2) <- cbind("vbkt","yli")
lagselect <- VARselect(DEU2,lag.max = 10, type = "const")
lagselect$selection 
#7 lagia


ESP <- cbind(inf1[,4],vbkt[,4],yli[,4])
lagselect <- VARselect(ESP,lag.max = 10, type = "const")
lagselect$selection
#5
ESP2 <- cbind(vbkt[,2],yli[,2])
colnames(ESP2) <- cbind("vbkt","yli")
lagselect <- VARselect(ESP2,lag.max = 10, type = "const")
lagselect$selection 
#7 lagia

EST <- cbind(inf1[,5],vbkt[,5],yli[,5])
lagselect <- VARselect(EST,lag.max = 10, type = "const")
lagselect$selection
#5
EST2 <- cbind(vbkt[,2],yli[,2])
colnames(EST2) <- cbind("vbkt","yli")
lagselect <- VARselect(EST2,lag.max = 10, type = "const")
lagselect$selection 
#7 lagia

FIN <- cbind(inf1[,6],vbkt[,6],yli[,6])
lagselect <- VARselect(FIN,lag.max = 10, type = "const")
lagselect$selection
#5
FIN2 <- cbind(vbkt[,2],yli[,2])
colnames(FIN2) <- cbind("vbkt","yli")
lagselect <- VARselect(FIN2,lag.max = 10, type = "const")
lagselect$selection 
#7 lagia


HUN <- cbind(inf1[,7],vbkt[,7],yli[,7])
lagselect <- VARselect(HUN,lag.max = 10, type = "const")
lagselect$selection
#5
HUN2 <- cbind(vbkt[,2],yli[,2])
colnames(HUN2) <- cbind("vbkt","yli")
lagselect <- VARselect(HUN2,lag.max = 10, type = "const")
lagselect$selection 
#7 lagia


ITA <- cbind(inf1[,8],vbkt[,8],yli[,8])
lagselect <- VARselect(ITA,lag.max = 10, type = "const")
lagselect$selection
#5
ITA2 <- cbind(vbkt[,2],yli[,2])
colnames(ITA2) <- cbind("vbkt","yli")
lagselect <- VARselect(ITA2,lag.max = 10, type = "const")
lagselect$selection 
#7 lagia


LUX <- cbind(inf1[,9],vbkt[,9],yli[,9])
lagselect <- VARselect(LUX,lag.max = 10, type = "const")
lagselect$selection
#5
LUX2 <- cbind(vbkt[,2],yli[,2])
colnames(LUX2) <- cbind("vbkt","yli")
lagselect <- VARselect(LUX2,lag.max = 10, type = "const")
lagselect$selection 
#7 lagia


SLO <- cbind(inf1[,10],vbkt[,10],yli[,10])
lagselect <- VARselect(SLO,lag.max = 10, type = "const")
lagselect$selection
#5
SLO2 <- cbind(vbkt[,10],yli[,10])
colnames(SLO2) <- cbind("vbkt","yli")
lagselect <- VARselect(SLO2,lag.max = 10, type = "const")
lagselect$selection
#7


SWE <- cbind(inf1[,11],vbkt[,11],yli[,11])
lagselect <- VARselect(SWE,lag.max = 10, type = "const")
lagselect$selection
#5

SWE2 <- cbind(vbkt[,11],yli[,11])
colnames(SWE2) <- cbind("vbkt","yli")
lagselect <- VARselect(SWE2,lag.max = 10, type = "const")
lagselect$selection
#8


#VAR mallien luonti

#varAUT <- VAR(AUT, p = 5, type = "const", season = NULL, exog = NULL)
#summary(varAUT)
varAUT2 <- VAR(AUT2, p = 8, type = "const", season = NULL, exog = NULL)
summary(varAUT2)

#varCZE <- VAR(CZE, p = 5, type = "const", season = NULL, exog = NULL)
#summary(varCZE)
varCZE2 <- VAR(CZE2, p = 7, type = "const", season = NULL, exog = NULL)
summary(varCZE2)

#varDEU <- VAR(DEU, p = 5, type = "const", season = NULL, exog = NULL)
#summary(varDEU)
varDEU2 <- VAR(DEU2, p = 7, type = "const", season = NULL, exog = NULL)
summary(varDEU2)

#varESP <- VAR(ESP, p = 5, type = "const", season = NULL, exog = NULL)
#summary(varESP)
varESP2 <- VAR(ESP2, p = 7, type = "const", season = NULL, exog = NULL)
summary(varESP2)

#varEST <- VAR(EST, p = 5, type = "const", season = NULL, exog = NULL)
#summary(varEST)
varEST2 <- VAR(EST2, p = 7, type = "const", season = NULL, exog = NULL)
summary(varEST2)

#varFIN <- VAR(FIN, p = 5, type = "const", season = NULL, exog = NULL)
#summary(varFIN)
varFIN2 <- VAR(FIN2, p = 7, type = "const", season = NULL, exog = NULL)
summary(varFIN2)

#varHUN <- VAR(HUN, p = 5, type = "const", season = NULL, exog = NULL)
#summary(varHUN)
varHUN2 <- VAR(HUN2, p = 7, type = "const", season = NULL, exog = NULL)
summary(varHUN2)

#varITA <- VAR(ITA, p = 5, type = "const", season = NULL, exog = NULL)
#summary(varITA)
varITA2 <- VAR(ITA2, p = 7, type = "const", season = NULL, exog = NULL)
summary(varITA2)

#varLUX <- VAR(LUX, p = 5, type = "const", season = NULL, exog = NULL)
#summary(varLUX)
varLUX2 <- VAR(LUX2, p = 7, type = "const", season = NULL, exog = NULL)
summary(varLUX2)

#varSLO <- VAR(SLO, p = 5, type = "const", season = NULL, exog = NULL)
#summary(varSLO)
varSLO2 <- VAR(SLO2, p = 7, type = "const", season = NULL, exog = NULL)
summary(varSLO2)

#varSWE <- VAR(SWE, p = 5, type = "const", season = NULL, exog = NULL)
#summary(varSWE)
varSWE2 <- VAR(SWE2, p = 8, type = "const", season = NULL, exog = NULL)
summary(varSWE)

#varAUT2 <- VAR(AUT2, p = 4, type = "const", season = NULL, exog = NULL)
#summary(varAUT2)
#autoplot(cbind(dvbkt[,1],dyli[,1]))

# sarjakorrelaatio

serial.test(varAUT, lags.pt = 12, type = "PT.asymptotic")
serial.test(varCZE, lags.pt = 12, type = "PT.asymptotic")
serial.test(varDEU, lags.pt = 12, type = "PT.asymptotic")
serial.test(varESP, lags.pt = 12, type = "PT.asymptotic")
serial.test(varEST, lags.pt = 12, type = "PT.asymptotic")
serial.test(varFIN, lags.pt = 12, type = "PT.asymptotic")
serial.test(varHUN, lags.pt = 12, type = "PT.asymptotic")
serial.test(varITA, lags.pt = 12, type = "PT.asymptotic")
serial.test(varLUX, lags.pt = 12, type = "PT.asymptotic")
serial.test(varSLO, lags.pt = 12, type = "PT.asymptotic")
serial.test(varSWE, lags.pt = 12, type = "PT.asymptotic")

#Heteroskedastisuus, eli onko volatitliteettia

arch.test(varAUT, lags.multi = 12, multivariate.only = TRUE)
arch.test(varCZE, lags.multi = 12, multivariate.only = TRUE)
arch.test(varDEU, lags.multi = 12, multivariate.only = TRUE)
arch.test(varESP, lags.multi = 12, multivariate.only = TRUE)
arch.test(varEST, lags.multi = 12, multivariate.only = TRUE)
arch.test(varFIN, lags.multi = 12, multivariate.only = TRUE)
arch.test(varHUN, lags.multi = 12, multivariate.only = TRUE)
arch.test(varITA, lags.multi = 12, multivariate.only = TRUE)
arch.test(varLUX, lags.multi = 12, multivariate.only = TRUE)
arch.test(varSLO, lags.multi = 12, multivariate.only = TRUE)
arch.test(varSWE, lags.multi = 12, multivariate.only = TRUE)

#normaalijakautuneen jäämät

normality.test(varAUT, multivariate.only = TRUE)
normality.test(varCZE, multivariate.only = TRUE)
normality.test(varDEU, multivariate.only = TRUE)
normality.test(varESP, multivariate.only = TRUE)
normality.test(varEST, multivariate.only = TRUE)
normality.test(varFIN, multivariate.only = TRUE)
normality.test(varHUN, multivariate.only = TRUE)
normality.test(varITA, multivariate.only = TRUE)
normality.test(varLUX, multivariate.only = TRUE)
normality.test(varSLO, multivariate.only = TRUE)
normality.test(varSWE, multivariate.only = TRUE)

#Test for model stability, no structural breaks
par(mfrow=c(1,1))
plot(stability(varAUT2,type = "OLS-CUSUM"))
plot(stability(varCZE2,type = "OLS-CUSUM"))
plot(stability(varDEU2,type = "OLS-CUSUM"))
plot(stability(varESP2,type = "OLS-CUSUM"))
plot(stability(varEST2,type = "OLS-CUSUM"))
plot(stability(varFIN2,type = "OLS-CUSUM"))
plot(stability(varHUN2,type = "OLS-CUSUM"))
plot(stability(varITA2,type = "OLS-CUSUM"))
plot(stability(varLUX2,type = "OLS-CUSUM"))
plot(stability(varSLO2,type = "OLS-CUSUM"))
plot(stability(varSWE2,type = "OLS-CUSUM"))

str(varEST2)
str(varFIN2)


#Granger kausaliteetti ja kointegraatio

#causality(varAUT, cause = "vbkt")
causality(varAUT2, cause = "vbkt")
causality(varAUT2, cause = "yli")

#causality(varCZE, cause = "vbkt...2.")
causality(varCZE2, cause = "vbkt")
causality(varCZE2, cause = "yli")

#causality(varDEU, cause = "vbkt...3.")
causality(varDEU2, cause = "vbkt")
causality(varDEU2, cause = "yli")

#causality(varESP, cause = "vbkt...4.")
causality(varESP2, cause = "vbkt")
causality(varESP2, cause = "yli")

#causality(varEST, cause = "vbkt...5.")
causality(varEST2, cause = "vbkt")
causality(varEST2, cause = "yli")

#causality(varFIN, cause = "vbkt...6.")
causality(varFIN2, cause = "vbkt")
causality(varFIN2, cause = "yli")

#causality(varHUN, cause = "vbkt...7.")
causality(varHUN2, cause = "vbkt")
causality(varHUN2, cause = "yli")

#causality(varITA, cause = "vbkt...8.")
causality(varITA2, cause = "vbkt")
causality(varITA2, cause = "yli")

#causality(varLUX, cause = "vbkt...9.")
causality(varLUX2, cause = "vbkt")
causality(varLUX2, cause = "yli")

#causality(varSLO, cause = "vbkt...10.")
causality(varSLO2, cause = "vbkt")
causality(varSLO2, cause = "yli")

#causality(varSWE, cause = "vbkt...11.")
causality(varSWE2, cause = "vbkt")
causality(varSWE2, cause = "yli")

#Impulssi responssi funktio

AUTirf1 <- irf(varAUT2, impulse = "vbkt", response = "yli", 
              n.ahead = 10, boot = TRUE)
plot(AUTirf1)
AUTirf2 <- irf(varAUT2, impulse = "yli", response = "vbkt", 
              n.ahead = 10, boot = TRUE)
plot(AUTirf2)
#Ei toimi

CZEirf1 <- irf(varCZE2, impulse = "vbkt", response = "yli", 
               n.ahead = 10, boot = TRUE)
plot(CZEirf1)
CZEirf2 <- irf(varCZE2, impulse = "yli", response = "vbkt", 
               n.ahead = 10, boot = TRUE)
plot(CZEirf2, ylab = "Velka/BKT", main = "Tsekki")


DEUirf1 <- irf(varDEU2, impulse = "vbkt", response = "yli", 
               n.ahead = 10, boot = TRUE)
plot(DEUirf1)
DEUirf2 <- irf(varDEU2, impulse = "yli", response = "vbkt", 
               n.ahead = 10, boot = TRUE)
plot(DEUirf2, ylab = "Velka/BKT", main = "Saksa")


ESPirf1 <- irf(varESP2, impulse = "vbkt", response = "yli", 
               n.ahead = 10, boot = TRUE)
plot(ESPirf1)
ESPirf2 <- irf(varESP2, impulse = "yli", response = "vbkt", 
               n.ahead = 10, boot = TRUE)
plot(ESPirf2,ylab = "Velka/BKT", main = "Espanja")


ESTirf1 <- irf(varEST2, impulse = "vbkt", response = "yli", 
               n.ahead = 10, boot = TRUE)
plot(ESTirf1)
ESTirf2 <- irf(varEST2, impulse = "yli", response = "vbkt", 
               n.ahead = 10, boot = TRUE)
plot(ESTirf2, ylab = "Velka/BKT", main = "Viro")


FINirf1 <- irf(varFIN2, impulse = "vbkt", response = "yli", 
               n.ahead = 10, boot = TRUE)
plot(FINirf1)
FINirf2 <- irf(varFIN2, impulse = "yli", response = "vbkt", 
               n.ahead = 10, boot = TRUE)
plot(FINirf2, ylab = "Velka/BKT", main = "Suomi")


HUNirf1 <- irf(varHUN2, impulse = "vbkt", response = "yli", 
               n.ahead = 10, boot = TRUE)
plot(HUNirf1)
HUNirf2 <- irf(varHUN2, impulse = "yli", response = "vbkt", 
               n.ahead = 10, boot = TRUE)
plot(HUNirf2, ylab = "Velka/BKT", main = "Unkari")


ITAirf1 <- irf(varITA2, impulse = "vbkt", response = "yli", 
               n.ahead = 10, boot = TRUE)
plot(ITAirf1)
ITAirf2 <- irf(varITA2, impulse = "yli", response = "vbkt", 
               n.ahead = 10, boot = TRUE)
plot(ITAirf2, ylab = "Velka/BKT", main = "Italia")


LUXirf1 <- irf(varLUX2, impulse = "vbkt", response = "yli", 
               n.ahead = 10, boot = TRUE)
plot(LUXirf1)
LUXirf2 <- irf(varLUX2, impulse = "yli", response = "vbkt", 
               n.ahead = 10, boot = TRUE)
plot(LUXirf2,ylab = "Velka/BKT", main = "Luxemburg")


SLOirf1 <- irf(varSLO2, impulse = "vbkt.", response = "yli", 
               n.ahead = 10, boot = TRUE)
plot(SLOirf1)
SLOirf2 <- irf(varSLO2, impulse = "yli", response = "vbkt", 
               n.ahead = 10, boot = TRUE)
plot(SLOirf2, ylab = "Velka/BKT", main = "Slovenia" )
#Positiivinen shokki velka/BKT:ssa johtaa ylijäämän kasvuun seuraavilla periodeilla


SWEirf1 <- irf(varSWE2, impulse = "vbkt.", response = "yli", 
               n.ahead = 10, boot = TRUE)
plot(SWEirf1)
SWEirf2 <- irf(varSWE2, impulse = "yli", response = "vbkt", 
               n.ahead = 10, boot = TRUE)
plot(SWEirf2, ylab = "Velka/BKT", main = "Slovenia" )
