#' Calculate Partial Pressure of Oxygen
#' 
#' @param tc Temperature in Celsius (well temperature), can be a vector
#' @param ap Atmospheric pressure in mmHg, can be a vector
#' @param method Character string: "legacy" (default) or "ideal"
#' @return Partial pressure of O2 in mmHg (Torr)
#' @export
#' @examples
#' O2(37, 760)                  # Uses legacy method (vp_poly + DO_approx)
#' O2(37, 760, method="ideal")  # Uses ideal method (vp_wagner + DO_weiss)
#' O2(c(25, 30, 37), 760)       # Vectorized
O2 <- Vectorize(
  function(tc, ap=760, method = "legacy") {
    method <- match.arg(method, c("legacy", "ideal"))
    
    # Constants
    MW_O2 <- 32.0
    Wt_H2O <- 1000.0
    MW_H2O <- 18.0
    TorrPerAtm <- 760.0
    
    # Calculate based on method
    henry <- HC(tc)
    
    if (method == "legacy") {
      vp <- vp_poly(tc)
      do_mgL <- DO_approx(tc, ap, vp)
    } else {
      vp <- vp_wagner(tc)
      do_mgL <- DO_weiss(tc, ap, vp)
    }
    
    do_molL <- (do_mgL / 1000.0) / MW_O2
    mol_frac <- do_molL / (Wt_H2O / MW_H2O)
    
    mol_frac * henry * TorrPerAtm
  },
  vectorize.args = c("tc")
)

