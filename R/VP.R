# ==============================================================================
# R/vapor_pressure.R
# ==============================================================================
#' Calculate Vapor Pressure - Polynomial Approximation
#' 
#' @param tc Temperature in Celsius
#' @return Vapor pressure in mmHg
#' @export
vp_poly <- function(tc) {
  X <- c(0.0456, -0.8559, 16.509)
  (X[1] * tc^2) + (X[2] * tc) + X[3]
}

#' Calculate Vapor Pressure - Wagner Equation
#' 
#' @param tc Temperature in Celsius
#' @return Vapor pressure in mmHg
#' @export
vp_wagner <- function(tc) {
  Tc <- 647.096
  pcMPa <- 22.064
  a1 <- -7.85951783
  a2 <-  1.84408259
  a3 <- -11.7866497
  a4 <- 22.6807411
  a5 <- -15.9618719
  a6 <-  1.80122502
  
  T <- tc + 273.15
  tau <- 1.0 - T / Tc
  
  ln_pr <- (Tc / T) *
    (a1 * tau +
       a2 * (tau^1.5) +
       a3 * (tau^3.0) +
       a4 * (tau^3.5) +
       a5 * (tau^4.0) +
       a6 * (tau^7.5))
  
  pMPa <- pcMPa * exp(ln_pr)
  pMPa * 7500.6168270417
}



