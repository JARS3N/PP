henry_constant <- function(Temp, precision = 'low') {
  X <- list(
    low = c(-0.0000058333, 0.0001821, 0.072405, 2.5443),
    high = c(
      -5.83333333333333e-06,
      0.000182142857142856,
      0.072404761904762,
      2.54428571428571
    )
  )[[precision]]

  (X[1]*Temp^3+X[2]*Temp^2+X[3]*Temp+X[4])*10000
}


