## find the continuously compounding monthly* rate which gives the tax-exempt payoff
## for TIPS, noms, and IBonds

TIPS_eff_rate_temp <- vector()
IBond_eff_rate_temp <- vector()
Nom_eff_rate_temp <- vector()


for(i in 1:(nrow(final.cash.values)-1)){
  mo <- min(120, nrow(final.cash.values)-i)
  if(final.cash.values$TIPSNT[i]>100){
TIPS_eff_rate_temp[i] <- log(final.cash.values$TIPSNT[i]/100)*(1/mo)
  }else{
    TIPS_eff_rate_temp[i] <-NA
}
}

for(i in 1:(nrow(final.cash.values)-1)){
  mo <- min(120, nrow(final.cash.values)-i)
  if(final.cash.values$NomNT[i]>100){
    Nom_eff_rate_temp[i] <- log(final.cash.values$NomNT[i]/100)*(1/mo)
  }else{
    Nom_eff_rate_temp[i] <-NA
  }
}


for(i in 1:(nrow(final.cash.values)-1)){
  mo <- min(120, nrow(final.cash.values)-i)
  if(final.cash.values$IBNT[i]>100){
    IBond_eff_rate_temp[i] <- log(final.cash.values$IBNT[i]/100)*(1/mo)
  }else{
    IBond_eff_rate_temp[i] <-NA
  }
}

## calculate the effective tax rate using the after-tax amount. 

etr_TIPS <- vector()
etr_Nom <- vector()
etr_IB <- vector()

for(i in 1:(nrow(final.cash.values)-1)){
  mo <- min(120, nrow(final.cash.values)-i)
  etr_TIPS[i] <- 1-log(final.cash.values$TIPSPayoff[i]/100)*(1/mo)*(1/TIPS_eff_rate_temp[i])
}

for(i in 1:(nrow(final.cash.values)-1)){
  mo <- min(120, nrow(final.cash.values)-i)
  etr_Nom[i] <- 1-log(final.cash.values$NomPayoff[i]/100)*(1/mo)*(1/Nom_eff_rate_temp[i])
}


for(i in 1:(nrow(final.cash.values)-1)){
  mo <- min(120, nrow(final.cash.values)-i)
  etr_IB[i] <- 1-log(final.cash.values$IBPayoff[i]/100)*(1/mo)*(1/IBond_eff_rate_temp[i])
}

etr_TIPS_norm <- (etr_TIPS/etr_Nom) * 0.35
etr_Nom_norm <- (etr_Nom/etr_Nom) * 0.35
etr_IB_norm <- (etr_IB/etr_Nom) * 0.35
