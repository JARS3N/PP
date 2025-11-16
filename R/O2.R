O2 <- function(tc, ap, method = "legacy") {
  method <- match.arg(method, c("legacy", "ideal"))
  
  # Constants
  MW_O2 <- 32.0
  Wt_H2O <- 1000.0
  MW_H2O <- 18.0
  TorrPerAtm <- 760.0
  
  # Calculate based on method
  henry <- HC(tc)  # units: atm/mol
  
  if (method == "legacy") {
    vp <- vp_poly(tc)
    do_mgL <- DO_approx(tc, ap, vp)
  } else {  # ideal
    vp <- vp_wagner(tc)
    do_mgL <- DO_weiss(tc, ap, vp)
  }
  
  # Final calculation (same for both methods)
  do_molL <- (do_mgL / 1000.0) / MW_O2     # units: mol/L
  mol_frac <- do_molL / (Wt_H2O / MW_H2O)  # units: moles
  
  mol_frac * henry * TorrPerAtm  # units: mmHg
}
