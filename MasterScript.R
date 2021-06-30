#### masterscript. Requires all other scripts in same directory to run.
#### ensure the "data" folder is a subset of the masterfile. 


# required packages:

  ## install packages
  install.packages('tidyverse')
  install.packages('lubridate')
  install.packages("ggplot2")
  
  
  ## load packages
  library(plyr)
  library(zoo)
  library(tidyverse)
  library(lubridate)
  library(ggplot2)

# run scripts in the director. An error for CPI-U TIPS Comp. is normal:

source("CPI-U Tips Comparison 2021-06-09.R")
  source("TIPS No Tax.R")
  source("TIPS - Nominal Comparison.R")
  source("I Bonds.R")
  
# make final cash values df
  
  final.cash.values <- as.data.frame(TIPSdf$Date)
  colnames(final.cash.values) <- "Date"
  final.cash.values$TIPSPayoff <- TIPSdf$Cash_to_mat
  final.cash.values$NomPayoff <- Nomdf$Payoffs
  final.cash.values$IBPayoff <- c(payoffIB, NA)
  final.cash.values$Inflation <- TIPSdf$Infl_to_end
  final.cash.values$Nom.TIPS.Spread <- final.cash.values$NomPayoff - final.cash.values$TIPSPayoff
  final.cash.values$TIPSNT <- TIPSNTdf$Cash_to_mat_NT
  final.cash.values$NomNT <- Nomdf$Payoffs_NT
  final.cash.values$IBNT <- IBonddf$IBNTPayoff
  final.cash.values$NomNT.TIPSNT.Spread <- final.cash.values$NomNT - final.cash.values$TIPSNT
  
  for(i in 7:9){
    temp <- (final.cash.values[,i-5] - 100)/(final.cash.values[,i] - 100)
    if(i == 7){
      final.cash.values$TIPS.tax.perc <- abs(1 - temp)
    }
    if(i == 8){
      final.cash.values$Nom.tax.perc <- abs(1 - temp)
    }
    if(i == 9){
      final.cash.values$IB.tax.perc <- abs(1-temp)
    }
  }
  
  
  
# best 10 year return

best.return <- vector()
best.return.NT <- vector()

  
  for(i in 1:(nrow(final.cash.values)-1)){
    if(max(final.cash.values$TIPSPayoff[i], final.cash.values$NomPayoff[i], final.cash.values$IBPayoff[i]) == final.cash.values$TIPSPayoff[i]){
      best.return[i] <- "TIPS"
    }
    if(max(final.cash.values$TIPSPayoff[i], final.cash.values$NomPayoff[i], final.cash.values$IBPayoff[i]) == final.cash.values$NomPayoff[i]){
      best.return[i] <- "Nominals"
    }
    if(max(final.cash.values$TIPSPayoff[i], final.cash.values$NomPayoff[i], final.cash.values$IBPayoff[i]) == final.cash.values$IBPayoff[i]){
      best.return[i] <- "I Bonds"
    }
  } 

  for(i in 1:(nrow(final.cash.values)-1)){
    if(max(final.cash.values$TIPSNT[i], final.cash.values$NomNT[i], final.cash.values$IBNT[i]) == final.cash.values$TIPSNT[i]){
      best.return.NT[i] <- "TIPS"
    }
    if(max(final.cash.values$TIPSNT[i], final.cash.values$NomNT[i], final.cash.values$IBNT[i]) == final.cash.values$NomNT[i]){
      best.return.NT[i] <- "Nominals"
    }
    if(max(final.cash.values$TIPSNT[i], final.cash.values$NomNT[i], final.cash.values$IBNT[i]) == final.cash.values$IBNT[i]){
      best.return.NT[i] <- "I Bonds"
    }
  } 
    
best.return <- as.factor(best.return)
best.return.NT <- as.factor(best.return.NT)

# run Effective Tax Rates script
  source("Effective Tax Rates.R")
  
# plots
  
  #plotting TIPS v Inflation
  plot(final.cash.values$Date, final.cash.values$TIPSPayoff, type="l", xaxt="n", xaxs="i", ylim=c(95,140),        # yields (amounts)
     ylab= "Final Payouts", xlab="Date", lwd=2)
  lines(final.cash.values$Date, final.cash.values$Inflation, type ="l", col="red", lwd=2)
  title("Figure 1: Investing $100: TIPS vs Inflation")
  abline(h=100, col="lightblue")
  legend("topright", legend=c("TIPS", "Inflation"), col=c("black","red"), lty=c(1,1), 
         lwd=c(2,2))
  axis.Date(1, at=seq(min(TIPSdf$Date), max(TIPSdf$Date), by="year"))
  

  # plotting cash balances at the end
  
  plot(final.cash.values$Date, final.cash.values$TIPSPayoff, type="l", xaxt="n", xaxs="i", ylim=c(95,140),        # yields (amounts)
       ylab= "Final Payouts", xlab="Date", lwd=2)
  title("Figure 2: Growth of $100 in TIPS, Nominal, IBond, Inflation")
  axis.Date(1, at=seq(min(TIPSdf$Date), max(TIPSdf$Date), by="year"))
  lines(final.cash.values$Date, final.cash.values$NomPayoff, type ="l", col="red", lwd=2)
  lines(final.cash.values$Date, final.cash.values$Inflation, type ="l", col="chartreuse3", lwd=2, lty=2)
  lines(final.cash.values$Date, final.cash.values$IBPayoff, type ="l", col = "blue", lwd=2)
  abline(h=100, col="lightblue")
  legend("topright", legend=c("TIPS","Nominal","I Bonds", "Inflation"), col=c("black","red","blue", "chartreuse3"), lty=c(1,1,1,2), 
         lwd=c(2,2,2,2))
  
  # plotting no tax
  plot(final.cash.values$Date, final.cash.values$TIPSNT, type="l", xaxt="n", xaxs="i", ylim=c(95,170),        # yields (amounts)
       ylab= "Final Payouts", xlab="Date", lwd=2)
  title("Payout of a $100 Tax Exempt Investment: TIPS, Nominal, IBond, Inflation")
  axis.Date(1, at=seq(min(TIPSdf$Date), max(TIPSdf$Date), by="year"))
  lines(final.cash.values$Date, final.cash.values$NomNT, type ="l", col="red", lwd=2)
  lines(final.cash.values$Date, final.cash.values$Inflation, type ="l", col="chartreuse3", lwd=2, lty=2)
  lines(final.cash.values$Date, final.cash.values$IBNT, type ="l", col = "blue", lwd=2)
  abline(h=100, col="lightblue")
  legend("topright", legend=c("TIPS","Nominal","I Bonds", "Inflation"), col=c("black","red","chartreuse3", "blue"), lty=c(1,1,1,2), 
         lwd=c(2,2,2,2))
  
  # plotting cash balances at end, but I Bond tax free
  plot(final.cash.values$Date, final.cash.values$TIPSPayoff, type="l", xaxt="n", xaxs="i", ylim=c(95,160),        # yields (amounts)
       ylab= "Final Payouts", xlab="Date", lwd=2)
  title("Growth of $100 in TIPS, Nominal, IBond (Tax Free), Inflation")
  axis.Date(1, at=seq(min(TIPSdf$Date), max(TIPSdf$Date), by="year"))
  lines(final.cash.values$Date, final.cash.values$NomPayoff, type ="l", col="red", lwd=2)
  lines(final.cash.values$Date, final.cash.values$Inflation, type ="l", col="chartreuse3", lwd=2, lty=2)
  lines(final.cash.values$Date, final.cash.values$IBNT, type ="l", col = "blue", lwd=2)
  abline(h=100, col="lightblue")
  legend("topright", legend=c("TIPS","Nominal","I Bonds", "Inflation"), col=c("black","red","blue", "chartreuse3"), lty=c(1,1,1,2), 
         lwd=c(2,2,2,2))
  
  
  # plotting the best performer
  
  plot(best.return, ylim = c(0, 160))
  title("Highest Payout, After-Tax")
  
  sum(best.return == "TIPS")/length(best.return)
  sum(best.return == "Nominals")/length(best.return)
  sum(best.return == "I Bonds")/length(best.return)
  
  
  # no tax
  
  plot(best.return.NT, ylim = c(0, 160))
  title("Highest Payout, Tax-Exempt")
  
  sum(best.return.NT == "TIPS")/length(best.return.NT)
  sum(best.return.NT == "Nominals")/length(best.return.NT)
  sum(best.return.NT == "I Bonds")/length(best.return.NT)
  
  
  # plot Percent of tax-exempt payoff that is taxed 
  plot(final.cash.values$Date, final.cash.values$TIPS.tax.perc, type="l", xaxt="n", xaxs="i", ylim=c(0.3,0.45),        # yields (amounts)
       ylab= "Effective Tax Rate", xlab="Date", lwd=2)
  title("Figure 3: Percent of Tax-Exempt Payoff that is Taxed")
  axis.Date(1, at=seq(min(TIPSdf$Date), max(TIPSdf$Date), by="year"))
  lines(final.cash.values$Date, final.cash.values$Nom.tax.perc, type ="l", col="red", lwd=2)
  lines(final.cash.values$Date, final.cash.values$IB.tax.perc, type ="l", col="blue", lwd=2)
  legend("topright", legend=c("TIPS","Nominal","I Bonds"), col=c("black","red","blue"), lty=c(1,1,1), 
         lwd=c(2,2,2))
  
## effective tax rate graphs:
  
  ## non-normalized graph
  plot(final.cash.values$Date, c(etr_TIPS,NA), type="l", xaxt="n", xaxs="i", ylim=c(0.3,0.35),        # yields (amounts)
       ylab= "Effective Tax Rate", xlab="Date", lwd=2)
  title("Figure 3: Effective Tax Rate")
  axis.Date(1, at=seq(min(TIPSdf$Date), max(TIPSdf$Date), by="year"))
  lines(final.cash.values$Date, c(etr_Nom,NA), type ="l", col="red", lwd=2)
  lines(final.cash.values$Date, c(etr_IB,NA), type ="l", col="blue", lwd=2)
  legend("bottomright", legend=c("TIPS","Nominal","I Bonds"), col=c("black","red","blue"), lty=c(1,1,1), 
         lwd=c(2,2,2))
  
  
  ## normalized graph. This should be more intuitive. Note that TIPS on occassion have higher effective
  ## tax rates towards the end of the sample due to the methodology taken (interest paid out in January/July).
  ## similarly, the effective tax rate calculation gives incorrect results for the final half-year for the I Bond
  ## since they are not designed to be taxed as bonds. 
  
  plot(final.cash.values$Date, c(etr_TIPS_norm,NA), type="l", xaxt="n", xaxs="i", ylim=c(0.3,0.35),        # yields (amounts)
       ylab= "Effective Tax Rate", xlab="Date", lwd=2)
  title("Figure 3: Effective Tax Rate")
  axis.Date(1, at=seq(min(TIPSdf$Date), max(TIPSdf$Date), by="year"))
  lines(final.cash.values$Date, c(etr_Nom_norm,NA), type ="l", col="red", lwd=2)
  lines(final.cash.values$Date, c(etr_IB_norm,NA), type ="l", col="blue", lwd=2)
  legend("bottomright", legend=c("TIPS","Nominal","I Bonds"), col=c("black","red","blue"), lty=c(1,1,1), 
         lwd=c(2,2,2))
  
  