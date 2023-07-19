O2<-Vectorize(function(TC,atm=760,percision='low'){
  mole_frac(TC,atm,percision)*atm
  })
