
#' Imputation Arguments

#'@description Function is in works. Included to collect all imputation arguments in one place.

## Arguments & Values -------------------------------------------------------------------------------
#' @param X A numeric vector, matrix, or data-frame of chemical concentration levels with n subjects and C chemicals to be imputed. Missing values are indicated by NA's.  Ideally, a numeric matrix.
#' @param DL The detection limit for each chemical as a numeric vector with length equal to C chemicals. Vector must be complete (no NA's); any chemical that has a missing detection limit is not imputed. If DL is a data-frame or matrix with 1 row or 1 column, it is forced as a numeric vector.
#' @param Z Any covariates used in imputing the chemical concentrations.  Ideally, a numeric matrix; however, Z can be a factor, vector, or data-frame. Assumed to be complete; observations with missing covariate variables are ignored in the imputation, with a warning printed. If none, enter NULL.
#' @param prior.coeff.mean The prior mean of number of covariates (p) x C coefficient matrix. The default, entered as NULL, will be a matrix of 1's, given by \code{\link[matrixNormal]{J}}.
#' @param prior.cov.mean  The prior mean of covariance matrix. The default, entered as NULL, is an identity matrix with size equal to the number of chemicals. Given by \code{\link[matrixNormal]{I}}.
#' @param initial  An optional two-item list that consists of initial values for the log imputed BDL values vectorized by subject in the Gibbs Sampler. The list contains two elements, one for each chain in the Gibbs Sampler. Each element is a vector of length n0C containing the log imputed BDL values vectorized by subject, (n0 is total # of missing values). If unknown for each chain, enter NA, and the initial values are automatically generated.
#' @param T Number of total iterations for the Gibbs Sampler. Defaults: 1000L.
#' @param n.burn The burn-in, which is the number of initial iterations to be discarded. Generally, the burn-in can be quite large as the imputed chemical matrices, X.imputed, are formed from the end of the chain -- the lowest state used is \eqn{T - 10*K}. Default is 1L (no burn-in).
#' @param K A natural number of imputed datasets to generate. Defaults: 5L.
#' @inheritParams estimate.wqs

#'@import matrixNormal
#'@return  nothing -- currently there is no function here.

impute.multivariate.bayesian <- function(X, DL, Z, prior.coeff.mean, prior.cov.mean, initial, T, n.burn, K, verbose){
  return()
}



