### First load and run the CPI-U Tips Comparison, Nominal COmparison, including packages

## import and arrange data
IBonds2 <- read.csv('./data/I bond yields.csv', stringsAsFactors = FALSE)
IBonds <- data.frame(IBonds2$Date.the.fixed.rate.was.set, IBonds2$Fixed.rate.for.bonds.issued.in.the.six.months.after.that.date, 
  IBonds2$Inflation.rate.for.six.months) 
colnames(IBonds) <- c("Date","Fixed_Rate","Inflation_Rate")


datesvec <- seq(as.Date("2003-01-01"), as.Date("2022-01-01"), by="months")

yieldsvec <- vector()
inflationvec <- vector()

for(i in 1:length(datesvec)){

 dummydate <- ymd(as.Date("2003-01-01")) %m+% months(i-1)
 yieldsvec[i] <- IBonds$Fixed_Rate[sum(IBonds$Date > as.Date(dummydate), na.rm = TRUE)+1]
 inflationvec[i] <- IBonds$Inflation_Rate[sum(IBonds$Date > as.Date(dummydate), na.rm = TRUE)+1]
  
}

yieldsvec <- yieldsvec[1:229]
inflationvec <- inflationvec[1:229]

IBonddf <- data.frame(datesvec, yieldsvec,inflationvec)
colnames(IBonddf) <- c("Dates","Fixed_Rate","Inflation_Rate")
IBonddf$Fixed_Rate <- as.numeric(IBonddf$Fixed_Rate)
IBonddf$Inflation_Rate <- as.numeric(IBonddf$Inflation_Rate)


#### Calculating payoff for an IBond

payoffIB <- vector()
inflvec <- vector()
payoffIBNT <- vector()

for(i in 1:(nrow(IBonddf)-1)){
  AIB <- 100                                            # set AIB
  tau <- 0.35                                           # set tax
  mat <- 10                                             # set maturity
  fixed.rate <- IBonddf$Fixed_Rate[i]/2                 # set yield from IBonddf
  prodvec <- vector()
  infltemp <- vector()
  jan_2022payment <- 0
  jan_2022payment_NT <- 0
  
  ## note that none of the bonds in question reach their maturity, although
  ## we pretend like they did for the purposes of comparison, so there is
  ## only the question of measuring bond value at 2022
  
  # payoff if bond reaches its full maturity
  if((i + mat*12) <= nrow(IBonddf)){
    for(j in 1:(mat*2)){ #no. 6 month periods until 2022-01-01, rounded down
      prodvec[j] <- max(1 + fixed.rate + (IBonddf$Inflation_Rate[i + (j-1)*6]) +
                          fixed.rate*(IBonddf$Inflation_Rate[i + (j-1)*6])/2, 1)
      infltemp[j] <- 1+(IBonddf$Inflation_Rate[i + (j-1)*6])
      jan2022payment <- 0
    }
  }
  
  # in-line formulas in what follows removed for graphical ease of comparison. The idea is that 
  # a bond is not disposed of in in January of 2022 and the taxpayer will be paid
  # the stated interest at a later date. This slightly overstates the "true" value. 
  # a more accurate representation is commented off: note the jagged edges 
  # produced by the graphs in the MasterScript. 
  
  if((i + mat*12) > nrow(IBonddf)){
    for(j in 1:max(1,(round_any(nrow(IBonddf) - i, 6, floor)/6))){  
      prodvec[j] <- max(1 + fixed.rate + (IBonddf$Inflation_Rate[i + (j-1)*6]) +
                          fixed.rate*(IBonddf$Inflation_Rate[i + (j-1)*6])/2, 1)
      infltemp[j] <- 1+(IBonddf$Inflation_Rate[i + (j-1)*6])
      if((i + (round_any(nrow(IBonddf) - i, 6, floor))) != nrow(IBonddf)){
      jan2022ratedummy <- (1 + fixed.rate + IBonddf$Inflation_Rate[i + (round_any(nrow(IBonddf) - i, 6, floor))] + 
                               fixed.rate*(IBonddf$Inflation_Rate[i + (round_any(nrow(IBonddf) - i, 6, floor))])/2)^(2/12) - 1

      jan_2022payment <- (1 + jan2022ratedummy)^(nrow(IBonddf) - i - (round_any(nrow(IBonddf) - i, 6, floor))) 
      jan_2022payment_NT <- (1 + jan2022ratedummy)^(nrow(IBonddf) - i - (round_any(nrow(IBonddf) - i, 6, floor))) 
      }
    }
  }
  
  prodvec <- as.numeric(prodvec)
  inflvec[i] <- prod(infltemp)
  
  if(nrow(IBonddf) - i >= 6){
    payoffIB[i] <- prod(prodvec)*AIB - (prod(prodvec)*AIB - AIB)*tau + max(0,(1-tau)*(jan_2022payment-1)*prod(prodvec)*AIB)
    payoffIBNT[i] <- prod(prodvec)*AIB + max(0, (jan_2022payment_NT-1)*prod(prodvec)*AIB)
  } else{
  payoffIB[i] <- jan_2022payment*AIB - (jan_2022payment*AIB-AIB)*tau
  payoffIBNT[i] <- jan_2022payment_NT*AIB
  }

}

IBonddf$IBATPayoff <- c(payoffIB,NA)
IBonddf$IBNTPayoff <- c(payoffIBNT,NA)
IBonddf$Tax <- IBonddf$IBNTPayoff - IBonddf$IBATPayoff


##################################################################################

# How is January 2022 treated? Do we get a payment there? We should! Check for nominals and TIPS too. 
# payment appears made for TIPS, nominal bonds, and I Bonds. 

##################################################################################


