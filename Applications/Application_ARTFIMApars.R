#---------------------------------------------------------------------------------
# Title: R code for the Estimation of Unknown Parameters for ARTFIMA Models 
#        and RSSs in the Application
# Author: Sepideh Mosaferi
# Date: April 2023
#---------------------------------------------------------------------------------
require("arfima"); require("artfima")

## two countries: Spain and France 

# read data

Spain <- read.table("/Users/sepidehmosaferi/Desktop/Spain.txt",header=TRUE)

France <- read.table("/Users/sepidehmosaferi/Desktop/France.txt",header=TRUE)

# RSS of fitting ARFIMA and ARTFIMA models 
nrow(Spain); nrow(France)
n <- 59

# Spain
SPA_GDPlog_LM <- arfima(Spain$Logx,order = c(0,0,0))
RSS_LMGDP <- sum((residuals(SPA_GDPlog_LM)$Mode1)^2); RSS_LMGDP

SPA_GDPlog_SLM <- artfima(Spain$Logx,arimaOrder=c(0,0,0),likAlg="Whittle")
RSS_SLMGDP <- sum(SPA_GDPlog_SLM$res^2); RSS_SLMGDP 

SPA_CO2log_LM <- arfima(Spain$Logy,order = c(0,0,0))
RSS_LMCO2 <- sum((residuals(SPA_CO2log_LM)$Mode1)^2); RSS_LMCO2

SPA_CO2log_SLM <- artfima(Spain$Logy,arimaOrder=c(0,0,0),likAlg="Whittle")
RSS_SLMCO2 <- sum(SPA_CO2log_SLM$res^2); RSS_SLMCO2 

# France
FRA_GDPlog_LM <- arfima(France$Logx,order = c(0,0,0))
RSS_LMGDP <- sum((residuals(FRA_GDPlog_LM)$Mode1)^2); RSS_LMGDP

FRA_GDPlog_SLM <- artfima(France$Logx,arimaOrder=c(0,0,0),likAlg="Whittle")
RSS_SLMGDP <- sum(FRA_GDPlog_SLM$res^2); RSS_SLMGDP 

FRA_CO2log_LM <- arfima(France$Logy,order = c(0,0,0))
RSS_LMCO2 <- sum((residuals(FRA_CO2log_LM)$Mode1)^2); RSS_LMCO2

FRA_CO2log_SLM <- artfima(France$Logy,arimaOrder=c(0,0,0),likAlg="Whittle")
RSS_SLMCO2 <- sum(FRA_CO2log_SLM$res^2); RSS_SLMCO2 

# Estimating d and lambda for x's
artfima(Spain$Logx,arimaOrder=c(0,0,0),likAlg="Whittle")
artfima(France$Logx,arimaOrder=c(0,0,0),likAlg="Whittle")

# Estimating d and lambda for y's
artfima(Spain$Logy,arimaOrder=c(0,0,0),likAlg="Whittle")
artfima(France$Logy,arimaOrder=c(0,0,0),likAlg="Whittle")

