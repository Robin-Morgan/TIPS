######### Running the yield script

payoffTIPSNT <- vector()
inflation_to_end <- vector()

for(i in 1:(nrow(TIPSdf)-1)){
  AIB <- 100                                            # set AIB
  month <- i - round_any(i-1, 12, floor)
  tau <- 0                                           # set tax
  ref_mo <- month
  mat <- 10                                             # set maturity
  end_year <- 2022                                      # set the end year 
  start_year <- 2003
  y_i <- start_year-1 + (round_any(i, 12, ceiling))/12         # what year does the purchase happen in? 
  if((end_year - y_i) >= mat){                                 # how many years until maturity OR end?
    t <- mat
  }  else{
    t <- end_year - y_i
  }
  months_from_start_whole_y <- (round_any(i, 12, ceiling) - 12)
  yield <- TIPSdf$Yield[i]                                            # set yield from TIPSdf
  coupon <- yield/200                                                 # divide by 2 and 100 to change from % to half-year 
  p_in_last_year <- as.logical(y_i - end_year + 1 == 0)    # was the instrument purchased in the last year of analysis?
  
  
  
  for(j in 1:t){                                                            # j is the (calendar) year being assessed. 
    ref_mo_year <- ref_mo + months_from_start_whole_y + (j-1)*12    # Bumps ref. mo up to the appl. year. e.g. "01" -> "01-2002"
    half_yr_july <- 7 + months_from_start_whole_y +(j-1)*12       # This takes the half-year payment and bumps it to appl. year. e.g. "07" -> "07-2002"
    full_yr_jan <- 13 + months_from_start_whole_y +(j-1)*12       # This takes the full-year interest payment and bumps it to appl. year e.g. "01" -> "01-2003"
    
    if(ref_mo>=1 & ref_mo<=6){
      #half year adj                                              # half year adjustmentwith tax
      int <- coupon*AIB*(1-tau)*(TIPSdf$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)]/TIPSdf$Ref_Index[ref_mo_year])
      hyadj <- int
      
      #full-year adj
      #interest
      intAIB <- coupon*AIB*(1-tau)*(TIPSdf$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/TIPSdf$Ref_Index[ref_mo_year])
      
      #half-year adjustment
      inthyadj <- coupon*hyadj*(1-tau)*(TIPSdf$Ref_Index[round_any(ref_mo_year, full_yr_jan,ceiling)]/
                                          TIPSdf$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)])
      
      infl_adj_hyadj <- hyadj*(TIPSdf$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/
                                 TIPSdf$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)])
      
      infl_adj_hyadj_tax <- (tau)*(hyadj*(TIPSdf$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/
                                            TIPSdf$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)])-hyadj)
      
      #AIB adjustment
      infl_adj_AIB <- (AIB*(TIPSdf$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/TIPSdf$Ref_Index[ref_mo_year]))
      infl_adj_AIB_tax <- (tau)*(AIB*(TIPSdf$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/TIPSdf$Ref_Index[ref_mo_year])-AIB)
      
      #final AIB adjustments
      AIB <- intAIB + inthyadj + infl_adj_hyadj - infl_adj_hyadj_tax + infl_adj_AIB - infl_adj_AIB_tax
      ref_mo <- 1
      
    } else{
      #full-year adj
      #interest
      intAIB <- coupon*AIB*(1-tau)*(TIPSdf$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/TIPSdf$Ref_Index[ref_mo_year])
      
      #AIB Inflation adjustment
      infl_adj_AIB <- (AIB*(TIPSdf$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/TIPSdf$Ref_Index[ref_mo_year]))
      infl_adj_AIB_tax <- (tau)*(AIB*(TIPSdf$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/TIPSdf$Ref_Index[ref_mo_year])-AIB)
      
      #Final AIB
      AIB <- intAIB + infl_adj_AIB - infl_adj_AIB_tax
      ref_mo <- 1
    }  
  }
  print(AIB)
  if(month>1 & month <7 & j == mat & (is.na(TIPSdf$Ref_Index[i + j*12])==FALSE)){ 
    ## inflate (and tax) up until the date of maturity, which is x years + month, for bonds purchased between Jan-June
    ## Bonds purchased Jan-July have no mid-year interest payment. 
    # test # 
    print("ACTIVE")
    
    infl_adj_AIB <- AIB*(TIPSdf$Ref_Index[full_yr_jan+month-1]/TIPSdf$Ref_Index[full_yr_jan])
    infl_adj_AIB_tax <- (tau)*(AIB*(TIPSdf$Ref_Index[full_yr_jan+month-1]/TIPSdf$Ref_Index[full_yr_jan])-AIB)
    
    AIB <- infl_adj_AIB - infl_adj_AIB_tax
    print(j)
    print(AIB)
  }
  
  if(month>=7 & j == mat & (is.na(TIPSdf$Ref_Index[i + j*12])==FALSE)){
    ## inflate and tax up until the date of maturity, for bonds purchased between July-Dec
    ## Bonds purchased July-Dec do have a mid-year interest payment, which itself is reinvested
    
    # HY interest payment
    int <- coupon*AIB*(1-tau)*(TIPSdf$Ref_Index[full_yr_jan+6]/TIPSdf$Ref_Index[full_yr_jan])
    hyadj <- int
    
    #maturity-adjustment for half year interest payment
    infl_adj_hyadj <- hyadj*(TIPSdf$Ref_Index[full_yr_jan+month-1]/
                               TIPSdf$Ref_Index[full_yr_jan+6])
    infl_adj_hyadj_tax <- (tau)*(hyadj*(TIPSdf$Ref_Index[full_yr_jan+month-1]/
                                          TIPSdf$Ref_Index[full_yr_jan+6])-hyadj)
    
    #maturity adjustment for AIB
    infl_adj_AIB <- (AIB*(TIPSdf$Ref_Index[full_yr_jan+month-1]/TIPSdf$Ref_Index[full_yr_jan]))
    infl_adj_AIB_tax <- (tau)*(AIB*(TIPSdf$Ref_Index[full_yr_jan+month-1]/TIPSdf$Ref_Index[full_yr_jan])-AIB)
    
    AIB <- infl_adj_hyadj + infl_adj_AIB - infl_adj_hyadj_tax - infl_adj_AIB_tax
    
    print("activey7-12")
    print(j)
    print(AIB)
  }
  
  payoffTIPSNT[i] <- AIB
  
  if((is.na(TIPSdf$Infl[i + mat*12])==FALSE)){
    inflation_to_end[i] <- TIPSdf$Infl[i+mat*12]/TIPSdf$Infl[i] * 100
  } else{
    inflation_to_end[i] <- TIPSdf$Infl[nrow(TIPSdf)]/TIPSdf$Infl[i]*100
  }
  
  after_tax_yield[i] <- log(AIB/100)/(min(mat*12, nrow(TIPSdf)-i))
  comp_inflation_rate[i] <- log(TIPSdf$Infl_to_end[i]/100)/(min(mat*12, nrow(TIPSdf)-i))
}

TIPSNTdf <- data.frame(TIPSdf$Date)
TIPSNTdf$Cash_to_mat_NT <- c(payoffTIPSNT, NA)
TIPSNTdf$Cash_to_mat_tax <- TIPSdf$Cash_to_mat
TIPSNTdf$Tax <- TIPSNTdf$Cash_to_mat_NT - TIPSNTdf$Cash_to_mat_tax
TIPSNTdf$Infl_to_end <- c(inflation_to_end, 100)
