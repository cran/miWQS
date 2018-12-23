# #'Accessory WQS Function: Initial Values for Training Set
# #'
# #'@family wqs
# #'@description Initial values for WQS model. Included for reproducibility.

# #'The default initial values are: \itemize{
# #' \item Intercept=0,
# #' \item b1=+- 0.1 , depending on if b1.pos=TRUE,
# #' \item equal weights (initial value)
# #' \item covariate estimates from covariate only model.
# #' }

# #'@note Adapted from \pkg{wqs}.
#
# #' @param Z covariates (passed as argument from \code{estimate.wqs})
# #' @param y outcome (passed as argument from \code{estimate.wqs})
# #' @param c Number of chemicals in X matrix.
# #' @param b1.pos passed from \code{estimate.wqs}.
# #' @param family distribution of y. See \code{\link[stats]{glm}} for description. Passed from \code{estimate.wqs}.
# #' @return A vector of initial values: \describe{
# #'    \item{b0}{the initial intercept value}
# #'    \item{b1}{ the initial chemical mixture effect value}
# #'    \item{w}{the initial weights. Default is 1/c, where c is the number of chemicals}
# #'    \item{z}{the initial covariates. Comes from glm2 ignoring the chemicals.}
# #'    }
# #'
# #'@examples
# #'\dontrun{
# #'     #No, 1, 2 covariates.
# #'set.seed(1213);
# #'specify.init( Z = NULL, y = rnorm(10),
# #'              b1.pos = TRUE, c = 4, family = "gaussian")
# #'set.seed(1213);
# #'specify.init( Z =  sample(c(0,1), 10, TRUE), y = rnorm(10),
# #'              b1.pos = TRUE, c = 4, family = "gaussian")
# #'set.seed(1213);
# #'specify.init( Z = cbind(sample(c(0,1), 10, TRUE), rnorm(10, 50, 5) ),
# #'              y = rnorm(10), b1.pos = TRUE, c = 4, family = "gaussian")
# #' #Use WQS Dataset to check
# #' #library(wqs);
# #' data("WQSdata")
# #' specify.init( y = WQSdata[,'y'], Z = WQSdata[ ,1],
# #'               b1.pos = TRUE, c = 7, family = "gaussian")
# #' specify.init( y = WQSdata[,'y'], Z = WQSdata[ , 1:2],
# #'               b1.pos = TRUE, c = 7, family = "gaussian")
# #' }
# #'
# # No need to be exported.

specify.init<-function (Z, y, b1.pos, c, family, offset, verbose) {
  w.0 <- rep(1/c, c); names(w.0) <- paste0("w", 1:c)  #weights
  b1.0 <- ifelse(b1.pos == TRUE, 0.1, -0.1)  #index

  #Initial Values for Covariates
  if (is.null(Z)) {
    init <- c(b0 = 0, b1 = b1.0, w.0)

  } else{
    #Covariate initial values come from glm.
    fit.init <- glm2(  y ~ ., data = data.frame(y, Z), family = family, offset = offset)  #Edited.
    init.Z <- coef(fit.init)[-1]  #or, if didn't work    #summary(fit.init)$coefficients[-1,1]
    p <- if( class(Z) == "numeric") { 1 } else { dim(Z)[2]  }  #Edited.
    names(init.Z) <- paste0("z", 1:p )
    #Intercept estimate comes from glm.
    b0.0 <- coef(fit.init)[1] ; names(b0.0) <- NULL
    init <- c(b0 = b0.0, b1 = b1.0, w.0, init.Z)
  }

  if(verbose){
    cat("## Initial Values for Training WQS model are \n")
    print(init)
  }

  return( init)
}

#Log
# Edited: Added line to find p if Z is a vector or matrix
# Edited: added family to glm2
# Edited: Change initial value for intercept to be estimate from covariate glm model.



