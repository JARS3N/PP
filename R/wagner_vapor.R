wagner_vapor <- function(T_C) {
  # Constants for water (Wagner & Pruss / IAPWS-97)
  Tc <- 647.096        # K (critical temperature)
  pc <- 22.064         # MPa (critical pressure)
  
  a1 <- -7.85951783
  a2 <-  1.84408259
  a3 <- -11.7866497
  a4 <- 22.6807411
  a5 <- -15.9618719
  a6 <-  1.80122502
  
  T   <- T_C + 273.15
  tau <- 1 - T / Tc
  
  # ln(p/pc) formulation
  ln_pr <- (Tc / T) * (
    a1 * tau +
      a2 * tau^(1.5) +
      a3 * tau^3 +
      a4 * tau^(3.5) +
      a5 * tau^4 +
      a6 * tau^(7.5)
  )
  
  p_MPa <- pc * exp(ln_pr)                 # saturation pressure in MPa
  p_mmHg <- p_MPa * 7500.6168270417        # 1 MPa = 7500.6168 mmHg
  
  # Outside the formal validity range (T < 273.16 K or T > Tc), return NA
  p_mmHg[ T < 273.16 | T > Tc ] <- NA_real_
  p_mmHg
}
