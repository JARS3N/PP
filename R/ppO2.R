O2<-Vectorize(function(TC,atm=760,percision='low'){
  #atm isn't used, the 760 is just unit conversion
  mole_frac(TC,atm,percision)*760
  })


O2_wag<-Vectorize(function(TC,atm=760){
  #atm isn't used, the 760 is just unit conversion
  mole_frac_wag(TC,atm)*760
  })

