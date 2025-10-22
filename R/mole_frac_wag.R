mole_frac_wag <- function(TC, atm,percision) {
  wt_H20 <- 1000
  mw_H20 <- 18
  mw_O2 <- 32
  DO <-
    dissolved_O2_wag(TC, atm, percision) * (mw_H20 / wt_H20 / 1000 / mw_O2)

  DO * henry_constant(TC, percision)
}
