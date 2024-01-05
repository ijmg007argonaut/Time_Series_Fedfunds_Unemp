options(warn=-1)
install.packages("ggplot2", "lubridate", "scales", "gridExtra")
install.packages("ggthemes", dependencies = TRUE)
install.packages("uroot", "urca")
install.packages("tseries")
install.packages("lmtest")
install.packages("vars", "astsa", "rugarch", "tsDyn")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("forecast")
install.packages("TSstudio")
install.packages('bayesforecast')
install.packages('tseries')
install.packages('zoo')
install.packages("aTSA")
library(tseries)
library(bayesforecast)
library(tidyverse)
library(forecast)
library(TSstudio)
library(tidyr)
library(tsDyn)
library(uroot)
library(ggplot2)
library(urca)
library(lmtest)
library(vars)
library(astsa)
library(rugarch)
library(zoo)
library(aTSA)
#######################################################################################################
# Set working directory
# NOTE: directory must be changed to run this R script locally
directory = "/media/ijmg/SSD_FOUR_TB/ACADEMICS_101/MY_PROJECTS/ADDED_PROJECTS/Time_Series_Federal_Funds_Rate_and_Unemployment_Rate/"
setwd(directory)
#######################################################################################################
# Verify federal funds rate time series
FedFunds_data <- read.csv('FEDFUNDS.csv')
print(head(FedFunds_data))
print(tail(FedFunds_data))
# Verify unemployment rate time series
UnEmpRate_data <- read.csv('UNRATE.csv')
print(head(UnEmpRate_data))
print(tail(UnEmpRate_data))
# Plot two time series
ggplot2::ggplot(FedFunds_data, ggplot2::aes(x=  as.Date(FedFunds_data$DATE)  ) )+ 
  ggplot2::geom_line(ggplot2::aes(y=FedFunds_data$FEDFUNDS, color="Federal Funds rate"), group=1) +
  ggplot2::geom_line(ggplot2::aes(y=UnEmpRate_data$UNRATE, color="Unemployment Rate"), group=1) +
  ggplot2::labs(color="Legend text") +
  ggplot2::labs(x = "Year", y = "Percent", title = "Federal Funds Rate and Unemployment Rate") +
  ggplot2::scale_color_manual(values=c("blue", "red")) +
  ggplot2::scale_x_date(date_breaks = "10 year", date_labels = "%Y")

# Decompose federal funds and unemployment time series 
FedFunds_ts <- ts(FedFunds_data$FEDFUNDS, start = c(1954, 7), end = c(2023, 3), frequency = 12)
FedFunds_ts_dec <- decompose(FedFunds_ts)
plot(FedFunds_ts_dec, xlab = "")
title(main = "Federal Funds Rate", line = 1.25)
title(xlab = "Year", line = 2.5) 

UnEmpRate_ts <- ts(UnEmpRate_data$UNRATE, start = c(1954, 7), end = c(2023, 3), frequency = 12)
UnEmpRate_ts_dec <- decompose(UnEmpRate_ts)
plot(UnEmpRate_ts_dec, xlab = "")
title(main = "Unemployment Rate", line = 1.25)
title(xlab = "Year", line = 2.5) 

# Time series summaries     
summary(FedFunds_ts)     
summary(UnEmpRate_ts) 

# Autocorrelations
acfCoreFF_ts<-acf(zoo::coredata(FedFunds_ts) )  
plot(acfCoreFF_ts, main="ACF for Federal Funds Rate")
axis(1, at = seq(0, 30, by=1), labels = 0:30 )

# Autocorrelations
acfCoreUNEMP_ts<-acf(zoo::coredata(UnEmpRate_ts) )  
plot(acfCoreUNEMP_ts, main="ACF for Unemployment Rate")
axis(1, at = seq(0, 30, by=1), labels = 0:30 )

# Autocorrelations
acfCoreFF_ts<-pacf(zoo::coredata(FedFunds_ts) )  
plot(acfCoreFF_ts, main="PACF for Federal Funds Rate")
axis(1, at = seq(0, 30, by=1), labels = 0:30 )

# Autocorrelations
acfCoreUNEMP_ts<-pacf(zoo::coredata(UnEmpRate_ts) )  
plot(acfCoreUNEMP_ts, main="PACF for Unemployment Rate")
axis(1, at = seq(0, 30, by=1), labels = 0:30 )

# Augmented Dickey Fuller
# Test for fed funds rate time series stationarity   
# alpha = 5% or 0.05 significance level
aTSA::stationary.test(FedFunds_ts, method = "adf", nlag =2)

# Phillips Perron Test
# Test for fed funds rate time series stationarity   
# alpha = 5% or 0.05 significance level
aTSA::stationary.test(FedFunds_ts, method = "pp", nlag =2)

# Kwiatkowski–Phillips–Schmidt–Shin Test
# Test for fed funds rate time series stationarity   
# alpha = 5% or 0.05 significance level
aTSA::stationary.test(FedFunds_ts, method = "kpss", nlag =2)

# Augmented Dickey Fuller
# Test for unemployment rate time series stationarity   
# alpha = 5% or 0.05 significance level
aTSA::stationary.test(UnEmpRate_ts, method = "adf", nlag =2)

# Phillips Perron Test
# Test for unemployment rate time series stationarity   
# alpha = 5% or 0.05 significance level
aTSA::stationary.test(UnEmpRate_ts, method = "pp", nlag =2)

# Kwiatkowski–Phillips–Schmidt–Shin Test
# Test for unemployment rate time series stationarity   
# alpha = 5% or 0.05 significance level
aTSA::stationary.test(UnEmpRate_ts, method = "kpss", nlag =2)

# Difference fed funds rate time series
# Compare two differencing strategies:
# Single stage: Difference only with lag = 2 to remove trend
# Two stage: Difference first with lag = 2 to remove trend,
#   then difference with lag = 12 for monthly seasonality
FF_ts_d2<-diff(FedFunds_ts, difference=2 ) 
FF_ts_d12<-diff(FedFunds_ts, difference=12 ) 
FF_ts_d2d12<-diff(FF_ts_d2, difference=12 ) 

d2CoreFF_ts<-acf(zoo::coredata(FF_ts_d2), main="Lag = 2 Differenced Federal Funds Rate")
d12CoreFF_ts<-acf(zoo::coredata(FF_ts_d12), main="Lag = 12 Differenced Federal Funds Rate")
d2d12CoreFF_ts<-acf(zoo::coredata(FF_ts_d2d12), main="Lag = 2 then 12 Differenced Federal Funds Rate")

# Difference unemployment rate time series
# Compare two differencing strategies:
# Single stage: Difference only with lag = 2 to remove trend
# Two stage: Difference first with lag = 2 to remove trend,
#   then difference with lag = 12 for monthly seasonality
UMEMP_ts_d2<-diff(UnEmpRate_ts, difference=2 ) 
UMEMP_ts_d12<-diff(UnEmpRate_ts, difference=12 ) 
UMEMP_ts_d2d12<-diff(UMEMP_ts_d2, difference=12 ) 

d2CoreUMEMP_ts<-acf(zoo::coredata(UMEMP_ts_d2), main="Lag = 2 Differenced Unemployment Rate")
d12CoreUMEMP_ts<-acf(zoo::coredata(UMEMP_ts_d12), main="Lag = 12 Differenced Unemployment Rate")
d2d12CoreUMEMP_ts<-acf(zoo::coredata(UMEMP_ts_d2d12), main="Lag = 2 then 12 Differenced Unemployment Rate")

# Cointegration Analysis
# System is composed of two time series: 
#   1. federal funds rate 
#   2. unemployment rate 
# Bind two time series into single data frame
dset <-  cbind(FedFunds_ts, UnEmpRate_ts)
head(dset)
# Perform Johansen Testing to determine number of cointegrating relationships 
#   present in the system
cointeg_test_rela <- urca::ca.jo(dset, type = "trace", ecdet ="const", K=2)
summary(cointeg_test_rela)

# Vector Error Correction Model (VECM)
VECM_model <- tsDyn::VECM(dset, 2, r=1, estim=("2OLS"))
summary(VECM_model)

# Impulse shock evaluations
# bind two time series into single data frame
dset <-  cbind(FedFunds_ts, UnEmpRate_ts)
# form vector autoregressive (VAR) model with p=2 lag order
FF_Unemp_VAR <- vars::VAR(dset, p=2, type="both")
# calculate impulse responses (IRF)
IRF_FedFunds_on_Umenp <- vars::irf(FF_Unemp_VAR, impulse = "FedFunds_ts", 
                             response="UnEmpRate_ts", n.head =200, ortho=FALSE)
plot(IRF_FedFunds_on_Umenp, ylab ="Umemployment Rate", 
     main="Response of Unemployment Rate to Shock from Federal Funds Rate")
IRF_Umenp_on_FedFunds <- vars::irf(FF_Unemp_VAR, impulse = "UnEmpRate_ts", 
                             response="FedFunds_ts", n.head =200, ortho=FALSE)
plot(IRF_Umenp_on_FedFunds, ylab ="Federal Funds Rate",
     main="Response of Federal Funds Rate to Shock from Unemployment Rate")



