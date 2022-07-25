#---------------------------------------------------------------------------------
# Title: R code for the Estimation of Unknown Parameters for ARTFIMA Models 
#        and MSEs in the Application
# Author: Sepideh Mosaferi
# Date: July 2022
#---------------------------------------------------------------------------------
require("arfima"); require("artfima")

## two countries: Spain and France 

# read data

Spain <- read.table("/Users/sepidehmosaferi/Desktop/Spain.txt")
colnames(Spain) <- c("logCO2","logGDP") 

France <- read.table("/Users/sepidehmosaferi/Desktop/France.txt")
colnames(France) <- c("logCO2","logGDP") 

# MSE of fitting ARFIMA and ARTFIMA models 
nrow(Spain); nrow(France)
n <- 59

# Spain
SPA_GDPlog_LM <- arfima(Spain$logGDP,order = c(0,0,0))
MSE_LMGDP <- sum((residuals(SPA_GDPlog_LM)$Mode1)^2); MSE_LMGDP

SPA_GDPlog_SLM <- artfima(Spain$logGDP,arimaOrder=c(0,0,0),likAlg="Whittle")
MSE_SLMGDP <- sum(SPA_GDPlog_SLM$res^2); MSE_SLMGDP 

SPA_CO2log_LM <- arfima(Spain$logCO2,order = c(0,0,0))
MSE_LMCO2 <- sum((residuals(SPA_CO2log_LM)$Mode1)^2); MSE_LMCO2

SPA_CO2log_SLM <- artfima(Spain$logCO2,arimaOrder=c(0,0,0),likAlg="Whittle")
MSE_SLMCO2 <- sum(SPA_CO2log_SLM$res^2); MSE_SLMCO2 

# France
FRA_GDPlog_LM <- arfima(France$logGDP,order = c(0,0,0))
MSE_LMGDP <- sum((residuals(FRA_GDPlog_LM)$Mode1)^2); MSE_LMGDP

FRA_GDPlog_SLM <- artfima(France$logGDP,arimaOrder=c(0,0,0),likAlg="Whittle")
MSE_SLMGDP <- sum(FRA_GDPlog_SLM$res^2); MSE_SLMGDP 

FRA_CO2log_LM <- arfima(France$logCO2,order = c(0,0,0))
MSE_LMCO2 <- sum((residuals(FRA_CO2log_LM)$Mode1)^2); MSE_LMCO2

FRA_CO2log_SLM <- artfima(France$logCO2,arimaOrder=c(0,0,0),likAlg="Whittle")
MSE_SLMCO2 <- sum(FRA_CO2log_SLM$res^2); MSE_SLMCO2 

# Estimating d and lambda for x's
artfima(Spain$logGDP,arimaOrder=c(0,0,0),likAlg="Whittle")
artfima(France$logGDP,arimaOrder=c(0,0,0),likAlg="Whittle")

# Estimating d and lambda for y's
artfima(Spain$logCO2,arimaOrder=c(0,0,0),likAlg="Whittle")
artfima(France$logCO2,arimaOrder=c(0,0,0),likAlg="Whittle")

