vapor_pressure <- function(Temp, percision = 'low') {
 # X = list(high = c(0.0455712421903711,-0.855936034546114,16.5093235942667),
  #low = c(0.0456,- 0.8559,16.509))[[percision]]
  X <- c(0.0456,- 0.8559,16.509)
  (X[1]*Temp^2)+(X[2]*Temp)+X[3]
}

weiss <- function(TC,ap) {
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
 C*((ap-pp::wagner_vapor(TC))/(760-pp::wagner_vapor(TC)))
}

