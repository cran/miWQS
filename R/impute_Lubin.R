#Impute.Lubin.R (subsections of RMD file from 2018May2014)
#' Lubin et al. 2004: Bootstrapping Imputation
#' @family imputation
#' @keywords imputation
#'
#' @description
#' Given lognormal interval-censored data between 0 and \emph{dlcol} , the function
#' creates a K = 5 imputed dataset using a bootstrap procedure as described in Lubin et al. 2004.

#' @details
#'   Lubin et al. (2004) evaluate several imputation approaches and show that a multiple imputation procedure
#'   using bootstrapping creates unbiased estimates and nominal confidence intervals unless the proportion of
#'   missing data is extreme. The authors coded the multiple imputation procedure in a SAS macro that
#'   is currently available. We converted the SAS macro into R code.
#'
#'   A chemical with missing values is imputed. The distribution for the interval-censored data \emph{chemcol} is assumed to be
#'   lognormal and censored between 0 and \emph{dlcol}. After bootstrapping, the values BDL are imputed
#'   using the inverse transform method. In other words, generate \eqn{u_i ~ Unif( 0.0001, dlcol)}
#'    and assign value \eqn{F^-1(u)} to \eqn{x_{i}} for \eqn{i = 1,...n_{0}} subjects with chemical values BDL.
#'
#' \code{impute.Lubin} performs the following:
#' \enumerate{
#'   \item Input arguments: chemical concentration, detection limit, and number of complete datasets.
#'   \item Obtain bootstrap samples.
#'   \item Generate weights vector.
#'   \item Use \code{\link[survival]{Surv}} function from Survival package to obtain survival object.
#'   \item Use \code{\link[survival]{survreg}} function from Survival package to obtain survival model.
#'   \item Sample from lognormal distribution with beta and variance from survival model as the parameters
#'         to obtain upper and lower bounds.
#'   \item Randomly generate value from uniform distribution between the previously obtained upper
#'         and lower bounds.
#'   \item Sample from the lognormal distribution to obtain the imputed data value associated with
#'         the above uniform value.
#'   \item Repeat for all observations. }
#'
#'   Observations with missing covariate variables are ignored.
#'   A matrix with n subjects and K imputed datasets is returned.
#'
#'
#' @note Code was adapted from Erin E. Donahue's original translation of the SAS macro developed from the paper.
#' @references
#' Lubin, J. H., Colt, J. S., Camann, D., Davis, S., Cerhan, J. R., Severson, R. K., … Hartge, P. (2004).
#' Epidemiologic Evaluation of Measurement Data in the Presence of Detection Limits. Environmental Health Perspectives,
#'  112(17), 1691–1696. https://doi.org/10.1289/ehp.7199

#' @param chemcol    A numeric vector, the chemical concentration levels of length c. Censored values are indicated by  NA. On original scale.
#' @param dlcol      The detection limit of the chemical. A value or a numeric vector of length c. Must be complete; a missing detection limit is ignored.
#' @param Z          Any covariates used in imputing \emph{chemcol}. Can be any R object but usually a
#'                   dataframe. If NULL, there are none.
# DEFUNCT #' @param seed       Sets the random seed. Defaults to NULL, the random process is initializes               if no seed is set.
#' @inheritParams   impute.univariate.bayesian.mi
                     #K or originally, n.bootstraps.
#' @inheritParams  estimate.wqs
                    #verbose.

#' @return A list of: \describe{
#' \item{X.imputed}{A n x K matrix of imputed X values.}
#' \item{bootstrap_index}{A n x K matrix of bootstrap indices selected for the imputation.}
#' \item{indicator.miss}{A check; the sum of imputed missing values above detection limit,
#'          which should be 0. Also printed to screen}
#' }
#'

#' @examples
#  Using abind::abind --possibly to impute multiple chemicals
#'   ###Example 2: Simulation
#'   #Let's apply the Lubin approach to the example simulated dataset.
#'   #A seed of 202 is executed before each run for reproducibility
#'   data(simdata87)
#'   #Impute: 1 chemical
#'   #No Covariates
#'   set.seed( 202)
#'   results_Lubin <- impute.Lubin (  simdata87$X.bdl[ ,1],   simdata87$DL[1],  K = 5)
#'   str(results_Lubin)
#'   summary(results_Lubin$imputed_values)
#'
#'   #1 Covariate
#'   set.seed( 202)
#'   sim.z1 <- impute.Lubin(simdata87$X.bdl[ ,1],   simdata87$DL[1],
#'                K = 5, Z = simdata87$Z.sim[ , 1])
#'   summary(sim.z1$imputed_values)
#'
#'   #2 Covariates
#'   set.seed( 202)
#'   sim.z2<- impute.Lubin(simdata87$X.bdl[ ,1],   simdata87$DL[1],
#'             K = 5, Z = simdata87$Z.sim[ , -2])
#'   summary(sim.z2$imputed_values)
#'   summary(sim.z2$bootstrap_index)

#' @import survival
#' @importFrom utils head
# #' @import stats
#'
#' @export impute.Lubin

impute.Lubin<-function(chemcol, dlcol,  Z = NULL, K = 5, verbose = FALSE){
    #require(survival)
    #set.seed(seed)

    ##Checks and Verifications --> Any modifications are returned: pmh, added
      l <- check_function.Lub(chemcol, dlcol, Z, K, verbose)
      DL <- l$DL
      Z <- l$Z
      if(verbose){
          cat("## First few covariates ... \n")
          print( utils::head(Z) )
      }
      K <- l$K

    ##creating dataset to be used in survival model.
      n <- length(chemcol)  #pmh, added since I removed it as parameter.
      #chemcol2: The observed concentration or the lower bound of the interval (i.e. 0.0001. The "time"; see survival::Surv.
      chemcol2 = ifelse(  is.na(chemcol), 0.00001, chemcol)
      #event:  Status indicator; 1=event at time, 3=interval censored. See Survival::surv.
      event =  ifelse(    is.na(chemcol), 3, 1)      #equivalent to ifelse(chemcol2 == 0.0001, )
      fullchem_data <- na.omit(
        data.frame(
          id = seq(1:n),
          chemcol2 = chemcol2,
          event = event,
          LB = rep(0, n),
          UB = rep(DL, n),
          Z
        ))
      n <- nrow(fullchem_data)  #redefine after removing missing values.
   if(verbose){
     print( utils::head(fullchem_data) )
     return(fullchem_data)
     }

    ##For loop to obtain bootstrap samples, weights vector, parameter estimates, and imputed values
    ##obtain parameter estimates for beta and sigma squared based on bootstrap sample using survreg.
    ##Impute analyte values based on sampling from lognormal(Beta, sigmasq)

    ## creating the empty matrices and vectors to be used in the for loop
      bootstrap_data<-matrix(0, nrow = n, ncol = K, dimnames = list(NULL, paste0("Imp.", 1:K) ) )
      # data_sample<-matrix(0,nrow = n, ncol = K)
      #  freqs<-matrix(0,nrow = n, ncol = 10)
      beta_not<- NA
      std<-rep(0,K)
      unif_lower<-rep(0,K)
      unif_upper<-rep(0,K)
      imputed_values<-matrix(0,nrow = n, ncol = K, dimnames = list(NULL, paste0("Imp.", 1:K) ) )

    for (a in 1:K){
       ##bootstrap
         #generate bootstrap samples by sampling row ID with replacement (indences )
         bootstrap_data[,a]<-as.vector(sample(1:n, replace = T))
         #select the data values to the bootstrap sample
         data_sample<- fullchem_data[bootstrap_data[,a],]

      ##obtain weights
        #Number of times each observation selected
        freqs<-as.data.frame(table(bootstrap_data[,a] ))
        #A vector of unique row IDs that are included in the sample
        freqs_ids<- as.numeric(as.character(freqs[ ,1]) )
        #create weights vector
        my.weights<-freqs[,2]

      ##survival analysis
        #ph-edited (removed my.weights) create dataset with observed rows and their weights
        final<-  fullchem_data[freqs_ids,]
        #creates response variable for the survreg function, includes z-values that are observed and intervals for those that are BDL
        my.surv.object <- survival::Surv( time = final$chemcol2, time2 = final$UB,
                                          event = final$event, type="interval")
        #pmh: fits parameteric survival regression model (includes covariates)
        model <- survival::survreg(my.surv.object ~ .,
                                   data = final[ , - (1:5), drop = FALSE],
                                   weights = my.weights, dist="lognormal",
                                   x = TRUE)
        beta_not  <- rbind(beta_not, model$coefficients)    #store the betas of each bootstrap sample
        std[a]<- model$scale                                #store the std devs of each bootstrap sample
        Z <- model$x                                        #rename Z to be the model matrix.

        ## Step 2 -- Impute the analyte valutes (pmh edited)
        mu <- Z %*% beta_not[a+1, ]
        unif_lower <-  plnorm(0.00001, mu, std[a])   #n x 1 vector
        unif_upper <-  plnorm(DL,      mu, std[a])   #n x 1 vector
        u <- runif( n,  unif_lower, unif_upper)
        imputed <-  qlnorm(u, mu, std[a])          #impute
        #Fully observed data: Replace the missing values with imputed; observed values should be same.
        imputed_values[  ,a] <- ifelse( fullchem_data$chemcol2 == 0.00001,  #if missing
                                        imputed,                      #replace BDLs with imputed values
                                        chemcol         #observed values.
        )

    }
      cat("## Check: Sum of values imputed that are above detection limit (upper limit; should be 0): \n")
      x.miss.index <- ifelse(   chemcol==0 | is.na(chemcol), TRUE, FALSE)
      indicator.miss <- sum( imputed_values[x.miss.index,  ] > DL)
      # missing.NA <- sum( !is.na(  data$X.bdl[ x.miss.index, ]) )   # Check whether all values in X.bdl are NA. The answer should be 0 (None are not NA).
      print(indicator.miss)

      if(verbose){        #ph added, useful for debugging and checking.
        # Check: print(model)
        beta_not <- beta_not[-1, , drop = FALSE]  #remove missing row
        cat("\n ## MLE Estimates \n")
        A <- format( cbind( beta_not, std), digits = 4, nsmall = 2 )
        colnames(A) <- c(names(model$coefficients), "stdev" )
        print(A)
        cat("## Uniform Imputation Range \n")
        B <- rbind( format( range(unif_lower),  digits = 3, scientific = TRUE),
                    format( range(unif_upper) , digits = 3, nsmall = 3)
                    )
        rownames(B) <- c("unif_lower", "unif_upper")
        print(B)
        cat("## Detection Limit:", unique(DL), "\n")
      }

  return( list(
    imputed_values = imputed_values,  #the imputed values.
      #possibly return also:
    bootstrap_index = bootstrap_data,   # list of bootstrap indices
    indicator.miss = indicator.miss  # Check: Sum of imputed  missing values above detection limit (should be 0):
   )
  )
}

##checks to make sure the arguments of impute.lubin are specified correctly.
check_function.Lub <- function( chemcol, dlcol, Z,  K, verbose){
      ##chemcol check
      if( !anyNA(chemcol) ) { stop( "chemcol has nothing to impute. No need to bootstrap.")}
      stopifnot( !is.null(chemcol) | is.vector(chemcol) )

      ##dlcol check
      detcol <- !is.na(chemcol)
      stopifnot(  !is.null(dlcol) | is.vector(dlcol)   )
      if( min(dlcol, na.rm = TRUE) == max(dlcol, na.rm = TRUE)){
        #All values of dlcol are the same so just pick 1.
        DL <- unique(dlcol)
      } else {
        warning(" The detection limit is not unique, ranging from ",
                min(dlcol, na.rm = TRUE), " to ", max(dlcol, na.rm = TRUE),
                "; The smallest value is assumed to be detection limit",
                call. = FALSE, immediate. = FALSE)
        #A summary
        cat("\n Summary when chemical is missing (BDL) \n")
        print( summary( dlcol[ detcol == 0] ))
        cat("## when chemical is observed \n ")
        print(summary( dlcol[ detcol == 1] ))

        #Assign Detection Limit:
        DL <- min(dlcol, na.rm = TRUE)           #if not, it is just the minimum of dlcol.
      }
      if(anyNA(DL) ){ stop( "The detection limit has missing values so chemical is not imputed.",
                            immediate.=FALSE )
        }

      ##Z checks
      if( is.null(Z) ){     #if Z is null, make it an intercept-term.
        Z <- matrix( rep(1, length(chemcol) ), ncol = 1)
        }
      if( anyNA(Z) ) {
        warning ( "Missing covariates are ignored.")
        }

      # K check
      if( !is.naturalnumber (K) ) { stop( "K must be a positive natural number)") }
      # if( !is.integer(K) ) {
      #  warning("K is not a natural number. The next largest is taken")
      #  K <- ceiling(K)
      #}

      ##return any modifications of any parameters
      return( list( DL = DL, Z = Z, K = K) )
}





###Save image
#```{r save_stuff }
# save.image("~/VCU Biostatistics/as work/Literature Papers/04_Lubin Detection Limit/impute_Lubin.RData")


#```
#Covariate Check
#if( !is.numeric(Z) ){ stop("Z must be a numeric") }
#  if( is.matrix(Z) | is.factor(Z) | is.data.frame(Z) ) {
#    p <- ncol(Z)
#} else{ if(is.factor(Z) | is.data.frame(Z) ) {
#    Z <- model.matrix( detcol ~ ., data = data.frame( detcol, Z)) [ , -1]
#   p <- ncol(Z)
#}  #
#  } else {
# }
##If detection limit is one number, make it a vector: pmh added
#dlcol <- if( length(dlcol) == 1) { rep(dlcol, length(chemcol)) }

#     if(anyNA(dlcol) ){
#        warning( "The detection limit has ", sum(is.na(dlcol)), " missing values, which will not be used.",
#                immediate. = FALSE)
#        dlcol <- na.omit(dlcol)
#     }

##detcol
#  stopifnot(  !is.null(detcol) | is.vector(detcol)   |  is.factor(detcol) )
# if(anyNA(detcol) ){
#    warning( "detcol has ", sum(is.na(detcol)), " missing values, which will not be used.",
#             immediate. = FALSE)
#   }
#   stopifnot( length(chemcol) == length(detcol) )



