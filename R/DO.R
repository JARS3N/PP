dissolved_O2<-function(TC,ap=atm){
  if(TC>=0 & TC<30){coef<-.678;adj<-35}else{if(TC>=30 & TC<=50){coef<-.827;adj<-49}}
  vp<-vapor_pressure(TC)
  ((ap-vp)*coef)/(adj+TC)
}


DO_split<-function (TC, ap,vp ) {
  if (TC >= 0 & TC < 30) {
    coef <- 0.678
    adj <- 35
  }else {
    if (TC >= 30 & TC <= 50) {
      coef <- 0.827
      adj <- 49
    }
  }
  ((ap - vp) * coef)/(adj + TC)
}

DO_weiss <- function(TC,ap,vp) {
  #dissolved oxygen in mg/L
  A1 = -139.34411
  A2 = 1.575701e5
  A3 = -6.642308e7
  A4 = 1.243800e10
  A5 = -8.621949e11
  T <- TC + 273.15
  lnC <- A1 + (A2 / T) + (A3 / T^2) + (A4 / T^3) + (A5 / T^4)
 C<- exp(lnC)  #mg/L
 # apply correction factor for localized atmospheric pressure
 C*((ap-vp)/(760-vp))
}
