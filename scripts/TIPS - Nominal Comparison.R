### First load and run the CPI-U Tips Comparison, including packages

## import and arrange data
NomDat <- read.csv('./data/10Y Nominals.csv', stringsAsFactors = FALSE)

# introduce TIPS yields

tmp3 <- NomDat %>% mutate(DATE = ymd(DATE))      # convert DATE to date format

tmp3$YIELD <- as.numeric(tmp3$YIELD)
Nomdf <- subset(tmp3, tmp3$DATE>=as.Date("2003-01-01") & tmp3$DATE <= as.Date("2021-01-01"))

#### Calculating payoff for a par bond 

ATPayoff <- vector()
NomPayoff_NT <- vector()

for(i in 1:(nrow(Nomdf)-1)){
  AIB <- 100                                            # set AIB
  tau <- 0.35                                           # set tax
  mat <- 10                                             # set maturity
  yield <- Nomdf$YIELD[i]/100                           # set yield from TIPSdf
  ATyield <- yield*(1-tau)
  rate_payment_2021 <- 0
  
  # payoff if bond reaches its full maturity
  if((i + mat*12) <= nrow(Nomdf)){
    t <- 2*mat
    ATPayoff[i] <- AIB * (1+ATyield/2)^(t)
    NomPayoff_NT[i] <- AIB * (1+yield/2)^(t)
  }
  
  # payoff if period ends before maturity reached
  if((i+ mat*12) > nrow(Nomdf)){
    t <-  round_any(nrow(Nomdf) - i, 6, floor)/6
    
    # final payout: 
    monthly_rate <- (1 + ATyield)^(1/12) - 1          # what is the monthly accrual of interest?
    
    # what was the compounded interest (rate) between the last interest payout and Jan 1 2021?
    rate_payment_2021 <- (1 + monthly_rate)^(nrow(Nomdf) - i - t*6)   
    
    # No Tax
    monthly_rate_NT <- (1 + yield)^(1/12) - 1
    rate_payment_2021_NT <- (1 + monthly_rate_NT)^(nrow(Nomdf) - i - t*6)
    
    if((i + (round_any(nrow(Nomdf) - i, 6, floor))) != 217){
    ATPayoff[i] <- AIB * (1+ATyield/2)^(t) + (AIB * (1+ATyield/2)^(t)) * (rate_payment_2021-1)
    NomPayoff_NT[i] <- AIB * (1+yield/2)^(t) + (AIB * (1+yield/2)^(t)) * (rate_payment_2021_NT-1)
    }else{
      ATPayoff[i] <- AIB * (1+ATyield/2)^(t)
      NomPayoff_NT[i] <- AIB * (1+yield/2)^(t) 
    }
  }

}

Nomdf$Payoffs <- c(ATPayoff, NA)
Nomdf$Payoffs_NT <- c(NomPayoff_NT, NA)
Nomdf$Tax <- Nomdf$Payoffs_NT - Nomdf$Payoffs
