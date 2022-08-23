install.packages("zoo")
install.packages("plyr")
library(plyr)
library(zoo)

Ref_Index <- c(100,103,103,104,104,107,107,108,108,111,112,
               113,114, 114, 114, 116, 117, 119,119, 119, 120, 120, 121,
               122, 122)

Infl <- c(104,104, 107, 107, 108, 108, 111, 112, 113, 114,
          114, 114, 116, 117, 119, 119, 119, 120, 120, 121,
          122, 122, 123, 123, 124)


## to make the sample df

y3 <- vector()
value <- Infl[25]

Ref_Index <- vector()

for(i in 1:12){
  value <- value + floor(runif(1, 0, 3))
  y3[i] <- value
}
  
Infl <- c(Infl,y3)

Ref_Index <- vector()


for(i in 4:length(Infl)){
  Ref_Index[1] <- 100
  Ref_Index[2] <- 103
  Ref_Index[3] <- 103
  Ref_Index[i] <- Infl[i-3]
}

Infl
Ref_Index

## making the dates

for(i in 1:13)
  paste(0,i,2000, sep="")

x <- c(1:13)

x[1:9] <- paste(0,x, sep="")

dat <- rep(NA,13)

temp <-

for(i in 1:12) {
  if(i<10){
  x <- paste(0,i, sep = "")
  temp <- paste(x, 2000, sep="-")
  dat[i] <- temp
  }
  if(i>=10){
    dat[i] <- paste(i, 2000, sep = "-")
  }
}

for(i in 13:24) {
  y <- i-12
  if(y<10){
    x <- paste(0,y, sep = "")
    temp <- paste(x, 2001, sep="-")
    dat[i] <- temp
  }
  if(y>=10){
    dat[i] <- paste(y, 2001, sep = "-")
  }
}

for(i in 25:36) {
  y <- i-24
  if(y<10){
    x <- paste(0,y, sep = "")
    temp <- paste(x, 2002, sep="-")
    dat[i] <- temp
  }
  if(y>=10){
    dat[i] <- paste(y, 2002, sep = "-")
  }
}



## the above can be streamlined by using a round-up general function to 12. 

dat2 <- vector()

for(i in 1:37){
  y <- round_any(i, 12, ceiling)
  z <- i - (y-12)
  if(z<10){
    x <- paste(0,z, sep = "")
    year <- 1999+ y/12
    temp <- paste(x, year, sep="-")
    dat2[i] <- temp
  }
  if(z>=10){
    year <- 1999 + y/12
    dat2[i] <- paste(z, year, sep = "-")
  }
}


## success!

## making the df

df <- as.data.frame(cbind(dat2, Ref_Index, Infl), stringsAsFactors =FALSE)
df$dat2 <- as.character(df$dat2)

df$dat2 <- as.Date(paste(df$dat2,"-01", sep=""), format = "%m-%Y-%d")
df
df$Ref_Index <- as.numeric(df$Ref_Index)
df$Infl <- as.numeric(df$Infl)

#### test running purchasing a par bond in January (2000-01-01). 
yield <- 0.05
coupon <- yield

for(i in 1:nrow(df))
  
#bond purchased Jan for $100
AIB <- 100
mo_pur <- 7
tau <- 0.35
ref_mo <- mo_pur

if(ref_mo>=1 & ref_mo<6){
  #half year adj
  int <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo, 7, ceiling)]/df$Ref_Index[ref_mo])
  hyadj <- int
  tax_int <- coupon*AIB*(tau)*(df$Ref_Index[round_any(ref_mo, 7, ceiling)]/df$Ref_Index[ref_mo])
  #full-year adj
    #interest
    intAIB <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[ref_mo])

    #half-year adjustment
    inthyadj <- coupon*hyadj*(1-tau)*(df$Ref_Index[round_any(ref_mo,13,ceiling)]/
                                      df$Ref_Index[round_any(ref_mo, 7, ceiling)])
    infl_adj_hyadj <- hyadj*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[round_any(ref_mo, 7, ceiling)])
    infl_adj_hyadj_tax <- (tau)*(hyadj*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[ref_mo])-hyadj)
    
    #AIB adjustment
    infl_adj_AIB <- (AIB*(df$Ref_Index[round_any(mo_pur, 13, ceiling)]/df$Ref_Index[mo_pur]))
    infl_adj_AIB_tax <- (tau)*(AIB*(df$Ref_Index[round_any(mo_pur, 13, ceiling)]/df$Ref_Index[mo_pur])-AIB)
    
  #final AIB adjustments
  AIB <- intAIB + inthyadj + infl_adj_hyadj - infl_adj_hyadj_tax + infl_adj_AIB - infl_adj_AIB_tax
  ref_mo <- 1
  
} else{
  #full-year adj
  #interest
  intAIB <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[ref_mo])
  
  #AIB Inflation adjustment
  infl_adj_AIB <- (AIB*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[ref_mo]))
  infl_adj_AIB_tax <- (tau)*(AIB*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[ref_mo])-AIB)
  
  #Final AIB
  AIB <- intAIB + infl_adj_AIB - infl_adj_AIB_tax
  ref_mo <- 1
}  

ex7mo <- AIB







######### now, try to create a for loop for each month in the first year of df ## success

payoffs <- vector()

for(i in 1:(nrow(df)-1)){
  AIB <- 100
  mo_pur <- i
  tau <- 0.35
  ref_mo <- mo_pur
  
  if(ref_mo>=1 & ref_mo<6){
    #half year adj
    int <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo, 7, ceiling)]/df$Ref_Index[ref_mo])
    hyadj <- int
    tax_int <- coupon*AIB*(tau)*(df$Ref_Index[round_any(ref_mo, 7, ceiling)]/df$Ref_Index[ref_mo])
    #full-year adj
    #interest
    intAIB <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[ref_mo])
    
    #half-year adjustment
    inthyadj <- coupon*hyadj*(1-tau)*(df$Ref_Index[round_any(ref_mo,13,ceiling)]/
                                        df$Ref_Index[round_any(ref_mo, 7, ceiling)])
    infl_adj_hyadj <- hyadj*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[round_any(ref_mo, 7, ceiling)])
    infl_adj_hyadj_tax <- (tau)*(hyadj*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[ref_mo])-hyadj)
    
    #AIB adjustment
    infl_adj_AIB <- (AIB*(df$Ref_Index[round_any(mo_pur, 13, ceiling)]/df$Ref_Index[mo_pur]))
    infl_adj_AIB_tax <- (tau)*(AIB*(df$Ref_Index[round_any(mo_pur, 13, ceiling)]/df$Ref_Index[mo_pur])-AIB)
    
    #final AIB adjustments
    AIB <- intAIB + inthyadj + infl_adj_hyadj - infl_adj_hyadj_tax + infl_adj_AIB - infl_adj_AIB_tax
    payoffs[i] <- AIB
    ref_mo <- 1
    
  } else{
    #full-year adj
    #interest
    intAIB <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[ref_mo])
    
    #AIB Inflation adjustment
    infl_adj_AIB <- (AIB*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[ref_mo]))
    infl_adj_AIB_tax <- (tau)*(AIB*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[ref_mo])-AIB)
    
    #Final AIB
    AIB <- intAIB + infl_adj_AIB - infl_adj_AIB_tax
    payoffs[i] <- AIB
    ref_mo <- 1
  }  

}

payoffs 

t <- 1
FinalAnnYTM <- (payoffs/100)^(1/t) - 1
FinalAnnInfl <- (df$Ref_Index[13]/Ref_Index[1:12])^(1/t) - 1



######### now, try to create a TWO year system. First for a purchase in Jan, then for a purchase in July
AIB <- 100
tau <- 0.35
ref_mo <- mo_pur
matyear <- 2
t <- matyear

### random thing, disregard for now
for(i in 1:(nrow(df)-1)){
  AIB <- 100
  mo_pur <- i
  tau <- 0.35
  ref_mo <- mo_pur
  matyear <- 2
  t <- matyear }
###

AIB <- 100
tau <- 0.35
mo_pur <- 1
ref_mo <- mo_pur
t <- 2  
yield <- 0.05
coupon <- yield

for(j in 1:t){
  ref_mo_year <- ref_mo+(j-1)*12    # This takes the reference year and bumps it up to the appl. year. e.g. "01" -> "01-2002"
  half_yr_july <- 7+(j-1)*12        # This takes the half-year payment and bumps it to appl. year. e.g. "07" -> "07-2002"
  full_yr_jan <- 13+(j-1)*12        # This takes the full-year interest payment and bumps it to appl. year e.g. "01" -> "01-2003"
    
if(ref_mo>=1 & ref_mo<=6){
  #half year adj
  int <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)]/df$Ref_Index[ref_mo_year])
  hyadj <- int

  #full-year adj
    #interest
  intAIB <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year])
  
    #half-year adjustment
  inthyadj <- coupon*hyadj*(1-tau)*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan,ceiling)]/
                                      df$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)])
  
  infl_adj_hyadj <- hyadj*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/
                             df$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)])
  
  infl_adj_hyadj_tax <- (tau)*(hyadj*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/
                                        df$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)])-hyadj)
  
    #AIB adjustment
  infl_adj_AIB <- (AIB*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year]))
  infl_adj_AIB_tax <- (tau)*(AIB*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year])-AIB)
  
    #final AIB adjustments
  AIB <- intAIB + inthyadj + infl_adj_hyadj - infl_adj_hyadj_tax + infl_adj_AIB - infl_adj_AIB_tax
  ref_mo <- 1
  
} else{
  #full-year adj
  #interest
  intAIB <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year])
  
  #AIB Inflation adjustment
  infl_adj_AIB <- (AIB*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year]))
  infl_adj_AIB_tax <- (tau)*(AIB*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year])-AIB)
  
  #Final AIB
  AIB <- intAIB + infl_adj_AIB - infl_adj_AIB_tax
  ref_mo <- 1
}  
}

payoffs <- AIB
FinalAnnYTM <- (payoffs/100)^(1/t) - 1
FinalAnnInfl <- (df$Infl[i]/df$Infl[1])^(1/t) - 1

### success!

############ Doing it for the entire df, storing the final AIB into a payoff() vector, 
############ 

payoffs <- vector()

for(i in 1:(nrow(df)-1)){
  AIB <- 100
  mo_pur <- i - (round_any(i, 12, ceiling) - 12)
  tau <- 0.35
  ref_mo <- mo_pur
  mat <- 2
  end_year <- 2002                                      # set the end year 
  y_i <- 1999 + (round_any(i, 12, ceiling))/12          # what year does the purchase happen in
  if((end_year - y_i) >= mat){                                 # how many years until maturity OR end?
    t <- mat
  }
  else{
    t <- end_year - y_i
  }
  whole_year_from_start_floor <- (round_any(i, 12, ceiling) - 12)
  yield <- 0.05
  coupon <- yield
  
  
  for(j in 1:t){                                                            # j is the (calendar) year being assessed. 
    ref_mo_year <- ref_mo + whole_year_from_start_floor + (j-1)*12    # Bumps ref. mo up to the appl. year. e.g. "01" -> "01-2002"
    half_yr_july <- 7 + whole_year_from_start_floor +(j-1)*12       # This takes the half-year payment and bumps it to appl. year. e.g. "07" -> "07-2002"
    full_yr_jan <- 13 + whole_year_from_start_floor +(j-1)*12       # This takes the full-year interest payment and bumps it to appl. year e.g. "01" -> "01-2003"
    
    if(ref_mo>=1 & ref_mo<=6){
      #half year adj
      int <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)]/df$Ref_Index[ref_mo_year])
      hyadj <- int
      
      #full-year adj
      #interest
      intAIB <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year])
      
      #half-year adjustment
      inthyadj <- coupon*hyadj*(1-tau)*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan,ceiling)]/
                                          df$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)])
      
      infl_adj_hyadj <- hyadj*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/
                                 df$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)])
      
      infl_adj_hyadj_tax <- (tau)*(hyadj*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/
                                            df$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)])-hyadj)
      
      #AIB adjustment
      infl_adj_AIB <- (AIB*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year]))
      infl_adj_AIB_tax <- (tau)*(AIB*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year])-AIB)
      
      #final AIB adjustments
      AIB <- intAIB + inthyadj + infl_adj_hyadj - infl_adj_hyadj_tax + infl_adj_AIB - infl_adj_AIB_tax
      ref_mo <- 1
      
    } else{
      #full-year adj
      #interest
      intAIB <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year])
      
      #AIB Inflation adjustment
      infl_adj_AIB <- (AIB*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year]))
      infl_adj_AIB_tax <- (tau)*(AIB*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year])-AIB)
      
      #Final AIB
      AIB <- intAIB + infl_adj_AIB - infl_adj_AIB_tax
      ref_mo <- 1
    }  
  }
  payoffs[i] <- AIB
}

inflation_rate_i_to_t <- vector()
inflation_rate_i_to_t <- df$Infl[25]/df$Infl
inflated_amount <- inflation_rate_i_to_t*100


## in the future: need to have a proper-end date infl adjustment for bonds which mature before the period ends.
## maybe make another if fork: if j = mat, then mature on the same month the bond was purchased, inflate to purchase month. 
## additionally, if the purchase month is between July-December, trigger another half-year interest payment. 
## this half-year interest payment must then be inflated. Again. 

test <- vector()

payoffstest <- vector()


for(i in 1:(nrow(df)-1)){
  AIB <- 100                                            # set AIB
  month <- i - round_any(i-1, 12, floor)
  tau <- 0.35                                           # set tax
  ref_mo <- month
  mat <- 1                                              # set maturity
  end_year <- 2003                                      # set the end year 
  y_i <- 1999 + (round_any(i, 12, ceiling))/12          # what year does the purchase happen in
  if((end_year - y_i) >= mat){                                 # how many years until maturity OR end?
    t <- mat
  }  else{
    t <- end_year - y_i
  }
  months_from_start_whole_y <- (round_any(i, 12, ceiling) - 12)
  yield <- 0.05                                            # set yield from DF
  coupon <- yield
  p_in_last_year <- as.logical(y_i - end_year + 1 == 0)    # was the instrument purchased in the last year of analysis?
  
  
  
  for(j in 1:t){                                                            # j is the (calendar) year being assessed. 
    ref_mo_year <- ref_mo + months_from_start_whole_y + (j-1)*12    # Bumps ref. mo up to the appl. year. e.g. "01" -> "01-2002"
    half_yr_july <- 7 + months_from_start_whole_y +(j-1)*12       # This takes the half-year payment and bumps it to appl. year. e.g. "07" -> "07-2002"
    full_yr_jan <- 13 + months_from_start_whole_y +(j-1)*12       # This takes the full-year interest payment and bumps it to appl. year e.g. "01" -> "01-2003"

    if(ref_mo>=1 & ref_mo<=6){
      #half year adj
      int <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)]/df$Ref_Index[ref_mo_year])
      hyadj <- int
        
      #full-year adj
      #interest
      intAIB <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year])
        
      #half-year adjustment
      inthyadj <- coupon*hyadj*(1-tau)*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan,ceiling)]/
                                          df$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)])
        
      infl_adj_hyadj <- hyadj*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/
                                 df$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)])
        
      infl_adj_hyadj_tax <- (tau)*(hyadj*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/
                                            df$Ref_Index[round_any(ref_mo_year, half_yr_july, ceiling)])-hyadj)
        
      #AIB adjustment
      infl_adj_AIB <- (AIB*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year]))
      infl_adj_AIB_tax <- (tau)*(AIB*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year])-AIB)
        
      #final AIB adjustments
      AIB <- intAIB + inthyadj + infl_adj_hyadj - infl_adj_hyadj_tax + infl_adj_AIB - infl_adj_AIB_tax
      ref_mo <- 1
        
    } else{
      #full-year adj
      #interest
      intAIB <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year])
        
      #AIB Inflation adjustment
      infl_adj_AIB <- (AIB*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year]))
      infl_adj_AIB_tax <- (tau)*(AIB*(df$Ref_Index[round_any(ref_mo_year, full_yr_jan, ceiling)]/df$Ref_Index[ref_mo_year])-AIB)
        
      #Final AIB
      AIB <- intAIB + infl_adj_AIB - infl_adj_AIB_tax
      ref_mo <- 1
      }  
  }
  print(AIB)
  if(month>1 & month <7 & j == mat & (is.na(df$Ref_Index[i + j*12])==FALSE)){ 
    ## inflate (and tax) up until the date of maturity, which is x years + month, for bonds purchased between Jan-June
    ## Bonds purchased Jan-July have no mid-year interest payment. 
    # test # 
    print("ACTIVE")
    
    infl_adj_AIB <- AIB*(df$Ref_Index[full_yr_jan+month-1]/df$Ref_Index[full_yr_jan])
    infl_adj_AIB_tax <- (tau)*(AIB*(df$Ref_Index[full_yr_jan+month-1]/df$Ref_Index[full_yr_jan])-AIB)
    
    AIB <- infl_adj_AIB - infl_adj_AIB_tax
    print(j)
    print(AIB)
  }
  
  if(month>=7 & j == mat & (is.na(df$Ref_Index[i + j*12])==FALSE)){
    ## inflate and tax up until the date of maturity, for bonds purchased between July-Dec
    ## Bonds purchased July-Dec do have a mid-year interest payment, which itself is reinvested
    
    # HY interest payment
    int <- coupon*AIB*(1-tau)*(df$Ref_Index[full_yr_jan+6]/df$Ref_Index[full_yr_jan])
    hyadj <- int
    
    #maturity-adjustment for half year interest payment
    infl_adj_hyadj <- hyadj*(df$Ref_Index[full_yr_jan+month-1]/
                               df$Ref_Index[full_yr_jan+6])
    infl_adj_hyadj_tax <- (tau)*(hyadj*(df$Ref_Index[full_yr_jan+month-1]/
                                          df$Ref_Index[full_yr_jan+6])-hyadj)
    
    #maturity adjustment for AIB
    infl_adj_AIB <- (AIB*(df$Ref_Index[full_yr_jan+month-1]/df$Ref_Index[full_yr_jan]))
    infl_adj_AIB_tax <- (tau)*(AIB*(df$Ref_Index[full_yr_jan+month-1]/df$Ref_Index[full_yr_jan])-AIB)
    
    AIB <- infl_adj_hyadj + infl_adj_AIB - infl_adj_hyadj_tax - infl_adj_AIB_tax
    
    print("activey7-12")
    print(j)
    print(AIB)
  }
  
  payoffstest[i] <- AIB
}

#### the above SEEMS okay. But note that there may be some weirdness around the 7 mo for a 1 year mat; 6<7, weirdly. 
# does this make sense? I suppose the final interest payment is on a larger sum. Tested on excel, worked out. 
# note that the algorithm first inflates, triggers interest, then TAXES inflation. Interest is taxed immediately. 
# This makes sense, since it more closely resembles paying taxes later in the year; the model assumes no deferral
# of any taxes owed in a given year, and there is normally a few months of deferral. This indirectly
# captures  deferral to a small extent. 























########### testing

AIB <- 100
mo_pur <- 1

#half year adj
int <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(mo_pur, 7, ceiling)]/df$Ref_Index[mo_pur])
hyadj <- int

#full-year adj
intAIB <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(mo_pur, 13, ceiling)]/df$Ref_Index[mo_pur])

inthyadj <- coupon*hyadj*(1-tau)*(df$Ref_Index[round_any(mo_pur,13,ceiling)]/
                                    df$Ref_Index[round_any(mo_pur, 7, ceiling)])

infl_adj_hyadj <- hyadj*(df$Ref_Index[round_any(mo_pur, 13, ceiling)]/df$Ref_Index[round_any(mo_pur, 7, ceiling)])
infl_adj_hyadj_tax <- (tau)*(hyadj*(df$Ref_Index[round_any(mo_pur, 13, ceiling)]/df$Ref_Index[mo_pur])-hyadj)

infl_adj_AIB <- (AIB*(df$Ref_Index[round_any(mo_pur, 13, ceiling)]/df$Ref_Index[mo_pur]))
infl_adj_AIB_tax <- (tau)*(AIB*(df$Ref_Index[round_any(mo_pur, 13, ceiling)]/df$Ref_Index[mo_pur])-AIB)

AIB <- intAIB + inthyadj + infl_adj_hyadj - infl_adj_hyadj_tax + infl_adj_AIB - infl_adj_AIB_tax

#calculating after-tax YTM

t <- 1
FinalAnnYTM <- (AIB/100)^(1/t) - 1
FinalAnnInfl <- (116/104)^(1/t)-1

FinalAnnYTM > FinalAnnInfl

#### someone purchased a bond after the mid-year interest payment, so on month 7
purchaseprice <- 100
AIB <- purchaseprice
mo_pur <- 7
ref_mo <- mo_pur
 
#full-year adj
#interest
intAIB <- coupon*AIB*(1-tau)*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[ref_mo])

#AIB Inflation adjustment
  infl_adj_AIB <- (AIB*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[ref_mo]))
  infl_adj_AIB_tax <- (tau)*(AIB*(df$Ref_Index[round_any(ref_mo, 13, ceiling)]/df$Ref_Index[ref_mo])-AIB)

#Final AIB
  AIB <- intAIB + infl_adj_AIB - infl_adj_AIB_tax
ref_mo <- 1


infl_adj <- (tau)*(AIB*(df$Ref_Index[round_any(mo_pur, 13, ceiling)]/df$Ref_Index[mo_pur])-AIB)


hyadj <- INT  
inthyadj <- coupon*hyadj*(1-tau)*(df$Ref_Index[round_any(mo_pur,12,ceiling)]/df$Ref_Index[round_any(mo_pur, 6, ceiling)])


df$Ref_Index[round_any(mo_pur,12,ceiling)]
df$Ref_Index[round_any(mo_pur, 6, ceiling)]






DatesasDates <- as.Date(Dates, format = "%m/%Y")

as.Date(as.character(20100512),format="%Y%m%d")
as.Date(as.character("2010/05/12"),format = "%Y/%m/%d")

date <- "2016-01-01"
date <- as.Date(date, format = "%Y-%m-%d")
date

as.yearmon(Dates, "%m-%Y")

as.yearmon("2001-01", )

DatesP <- paste(Dates,"/01", sep="")
as.Date(as.character(DatesP), format="%m/%Y/%d")
