O2<-Vectorize(function(TC,atm=760,percision='low'){
  #atm isn't used, the 760 is just unit conversion
  mole_frac(TC,atm,percision)*760
  })

