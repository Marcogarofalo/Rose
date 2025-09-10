AIC <- function(v, err, chi2dof, dof, npar, multiplicity = 1) {
  l <- list()
  Nmeas <- dof + npar
  AIC <- exp(-0.5 * (chi2dof * dof + 2 * npar - Nmeas)) / multiplicity
  N <- sum(AIC)
  AIC <- AIC / N
  l$AIC <- AIC
  l$m <- sum(v * AIC)
  stat <- sum(AIC * err^2)
  syst <- sum(AIC * (v - l$m)^2)
  l$dm <- sqrt(stat + syst)
  l$stat <- sqrt(stat)
  l$syst <- sqrt(syst)
  return(l)
}
BAIC <- function(v, err, chi2dof, dof, npar, multiplicity = 1) {
  l <- list()
  Nmeas <- dof + npar
  AIC <- exp(-0.5 * (chi2dof * dof + 2 * npar - 2 * Nmeas)) / multiplicity
  N <- sum(AIC)
  AIC <- AIC / N
  l$AIC <- AIC
  l$m <- sum(v * AIC)
  stat <- sum(AIC * err^2)
  syst <- sum(AIC * (v - l$m)^2)
  l$dm <- sqrt(stat + syst)
  l$stat <- sqrt(stat)
  l$syst <- sqrt(syst)
  return(l)
}
