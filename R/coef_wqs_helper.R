#'Finding WQS Coefficients
#'
#'@name coef.wqs
#'@family wqs
#'@seealso \code{\link[stats]{coef}}
#'@keywords wqs
#'
#'@description An accessor function that returns the coefficients from the validation WQS model,
#'a glm2 object.
#'
#'@param object Object of class WQS, which is output using \code{estimate.wqs}. Or object from class
#'miWQS, which is an output using \code{pool.wqs}.
      #previously W. Need argument names to match up with default methods (see stats::coef)
#'@inheritDotParams stats::coef  -object

#'@examples
#' #Use simulated dataset and set seed for reproducibility.
#' data(simdata87)
#' set.seed(23456)
#' Wa <- estimate.wqs( y = simdata87$y.scenario, X = simdata87$X.true,
#'                   B = 10, family = "binomial")
#' coef(Wa)

#'@importFrom stats coef
#'@export
coef.wqs<-function(object,  ...){
  #Check if
  if(class(object) == "wqs"){
    coef(object$fit,  ...)  #generic function in stats.
  }
  else { stop("object is not from class `wqs`.")}
}


# @rdname coef.wqs
# @export coef.miWQS
# coef.miWQS <- function(object, ...){
#  if(class(object) == "miWQS"){
#    fit <- object$pooled.wqs.estimates$pooled.mean
#    names(fit) <- dimnames(object$pooled.wqs.estimates)[[1]]
#    return(fit)
#  }
#}
