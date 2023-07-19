moles<-function(TC,atm,percision='low'){
  wt_H2O <- 1000 # g/L
  mw_H2O <- 18 # g/mol
  O2(TC,atm,percision)/atm/henry_constant(TC,percision)*wt_H2O/mw_H2O
}
