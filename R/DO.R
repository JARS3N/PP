dissolved_O2<-function(TC,ap=atm){
  if(TC>=0 & TC<30){coef<-.678;adj<-35}else{if(TC>=30 & TC<=50){coef<-.827;adj<-49}}
  vp<-vapor_pressure(TC)
  ((ap-vp)*coef)/(adj+TC)
}

