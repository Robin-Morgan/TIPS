### Yearly to Monthly Conversions

yearly_r_to_mo <- function(yearly_rate){
  monthly_rate <- (1 + yearly_rate)^(1/12)
  return(monthly_rate)
}

test <- yearly_r_to_mo(0.05)