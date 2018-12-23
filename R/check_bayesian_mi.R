## Check for proper execution of Bayesian imputation function(s)

# #'@importFrom makeJournalTables is.naturalnumber

check_bayesian_mi <- function(X, DL, Z, T, n.burn, K, verbose) {
  #Check X
  if( !anyNA(X) ) { stop("Matrix X has nothing to impute. No need to impute.")}
  if( class(X) == "data.frame") {  X <- as.matrix(X) }   #Can convert a numeric dataframe X into a matrix.
  stopifnot( all ( apply(X, 2, class) == "numeric") )    #X must be numeric matrix

  #Check DL
  if( is.data.frame(DL) | is.matrix(DL) ) {
    if(ncol(DL) > 1 & nrow(DL) > 1) { stop("The detection limit must be a vector.")}
    DL <-  as.numeric(DL)
  }
  if( anyNA(DL) ) {
    no.DL <- which(is.na(DL))
    warning("The detection limit for ", names(no.DL), " is missing and is removed in imputation.")
    #Remove missing
    DL <- DL [ -no.DL ]
    if(! (names(no.DL) %in% colnames(X) ) ) { cat("  ##Missing DL does not match colnames(X) \n") }   #a check
    X <- X[ , -no.DL]
  }

  #Check Z: Create a model matrix
  stopifnot( nrow(X) == nrow(Z) )
  if(is.null(Z)){ Z <- matrix(1, nrow = nrow(X), ncol = 1)}
  if(is.data.frame(Z) | is.list(Z) ) {
    if( anyNA(Z) ) { warning ( "Missing covariates are ignored.")  }
    Z <- model.matrix(  ~ . , model.frame(Z) )
    #print(head(Z))  #tmp
  }

  #Checking n.burn and T
  stopifnot( is.numeric(n.burn))
  stopifnot( is.numeric(T))
  if( !(n.burn < T) ) { stop("Burn-in is too large; burn-in must be smaller than MCMC iterations T")   }

  #K check
  if( K <= 0) {  stop( "K must be a positive natural number)")     }
  if( !is.naturalnumber(K) ) {
    warning("K is not a natural number. The next largest is taken")
    K <- ceiling(K)
  }

  #Return items that changed:
  return( list( X = X, DL = DL, Z = Z, K = K))
}

# In the main function type
# check <- check_bayesian_mi(X, DL, Z, T, n.burn, K, verbose)
# X <- check$X
# DL <- check$DL
# Z  <- check$Z
# K <- check$K





















