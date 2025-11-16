# ==============================================================================
# R/henry.R
# ==============================================================================
#' Calculate Henry's Law Constant
#' 
#' @param tc Temperature in Celsius
#' @return Henry's constant in atm/mol
#' @export
HC <- function(tc) {
  ((-0.0000058333 * (tc^3.0)) +
     (0.0001821     * (tc^2.0)) +
     (0.072405      * tc) +
     2.5443) * 10000.0
}
