######################################################################

# file: maxGroupSeq_helperFunction.R

# Purpose: split a sequence of numbers into homogenous groups as much as possible and calculate the remaining groupo sizes

######################################################################

maxGroupSeq <- function(N, group_n = 30){
  ## get the maximum number of complete groups given group_n
  max_group <- floor(N/group_n)
  
  ## get the remaining
  remaining <- (N - (max_group * group_n))
  
  ## make the number sequence
  result <- c(seq(1, (max_group * group_n), group_n), seq((max_group * group_n), N, remaining))
  
  return(result)
}
