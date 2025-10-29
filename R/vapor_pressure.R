vapor_pressure <- function(Temp) {
 # X = list(high = c(0.0455712421903711,-0.855936034546114,16.5093235942667),
  #low = c(0.0456,- 0.8559,16.509))[[percision]]
  X <- c(0.0456,- 0.8559,16.509)
  (X[1]*Temp^2)+(X[2]*Temp)+X[3]
}



vp_poly <- function(Temp) {
  X <- c(0.0456,- 0.8559,16.509)
  (X[1]*Temp^2)+(X[2]*Temp)+X[3]
}

vp_wagner<-function(Temp){


 }
