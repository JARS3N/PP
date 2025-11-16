# ==============================================================================
# R/dissolved_oxygen.R
# ==============================================================================
#' Calculate Dissolved Oxygen - Piecewise Approximation
#' 
#' @param tc Temperature in Celsius
#' @param ap Atmospheric pressure in mmHg
#' @param vp Vapor pressure in mmHg
#' @return Dissolved oxygen in mg/L
#' @export
DO_approx <- function(tc, ap, vp) {
  if (tc >= 0 && tc < 30) {
    coef <- 0.678
    adj <- 35
  } else if (tc >= 30 && tc <= 50) {
    coef <- 0.827
    adj <- 49
  } else {
    stop("Temperature out of range [0, 50] for approx method")
  }
  ((ap - vp) * coef) / (adj + tc)
}

#' Calculate Dissolved Oxygen - Weiss Equation
#' 
#' @param tc Temperature in Celsius
#' @param ap Atmospheric pressure in mmHg
#' @param vp Vapor pressure in mmHg
#' @return Dissolved oxygen in mg/L
#' @export
DO_weiss <- function(tc, ap, vp) {
  A1 <- -139.34411
  A2 <- 157570.1
  A3 <- -66423080.0
  A4 <- 1.2438e10
  A5 <- -8.621949e11
  
  T <- tc + 273.15
  invT <- 1.0 / T
  
  lnC <- A1 + A2*invT + A3*(invT^2.0) + A4*(invT^3.0) + A5*(invT^4.0)
  C <- exp(lnC)
  C * ((ap - vp) / (760.0 - vp))
}

