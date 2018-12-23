#'Accessory WQS Function: Making Quantiles of Correlated Index
#(Added 8/22/17)
#'
#'@family wqs
#'@seealso \code{\link[stats]{quantile}}
#'@keywords wqs

#'@description \code{make.quantile.matrix} makes quantiles from a matrix. If the matrix has values missing between 0 and some threshold, say the detection limit, all these missing values  (indicated by NA) go into the first quantile.
#'
#'@details
#'  Accessory functions for WQS. Produces sample quantiles for a matrix \emph{x} using \code{\link[stats]{quantile}} function. Names are kept and the 7th quantile algorithm is used. When there is missing data (as indicated by NA's), \code{make.quantile.matrix} places all of the censored data into the first quantile. The remaining quantiles are evenly spread over the observed data. A printed message is displaced what the function does.
#'
#' @note The \code{make.quantile.matrix} is a helper function in \pkg{wqs}.
#'
#' @param x       A numeric matrix. Any missing values are indicated by NA's.
#' @inheritParams estimate.wqs
                  # n.quantiles, verbose: passed from estimate.wqs
#' @param ... Further arguments passed to or from other methods. Currently has no effect.
# #' @inheritDotParams stats::quantile

#' @examples
#' #Example 1: Make quantiles for first nine chemicals using complete chemical data
#' data(simdata87)
#' q <- make.quantile.matrix( simdata87$X.true[ , 1:9], 4)
#' summary(q)
#'
#' #Example 2: Place missing values of first nine chemicals in first quantiles
#' q2 <- make.quantile.matrix( simdata87$X.bdl[  , 1:9], 4, verbose = TRUE)
#' summary(q2)
#'
#' @import stats
#' @export

make.quantile.matrix <- function (x, n.quantiles, verbose = FALSE,  ...) {
  #x is a matrix so call it as such.
  matrix <- x

  #Check: x must be numeric
  no.not.numeric <- sum( apply(matrix, 2, class) != "numeric")
  a <- ifelse( no.not.numeric == 0, "",
          stop("At least one chemical in x is not numeric")
  )

  if( !anyNA(matrix)){
    cat("## No missing values in matrix detected. Regular Quantiles computed.\n")

    #Find the quantile for each columns
    q <- matrix(0, dim(matrix)[1], dim(matrix)[2])
    I <- dim(matrix)[2]
    for (i in 1:I) {
      xq <- stats::quantile(matrix[, i],  probs = c(0:n.quantiles/n.quantiles),
                     na.rm = FALSE, names = TRUE, type = 7,  ...)
      q[, i] <- cut(matrix[, i], breaks = xq, include.lowest = TRUE)

    }
    q <- q - 1

  } else{
    cat("## All BDLs are placed in the first quantile \n")
    q <- matrix(-10, dim(matrix)[1], dim(matrix)[2])
    I <- dim(matrix)[2]

    for (i in 1:I) {
      #For those observed, divide up the quantiles - 1.
      xq <- stats::quantile( matrix[ , i] ,  probs = c(0:(n.quantiles-1)/(n.quantiles-1)) ,
                      na.rm = TRUE,   names = TRUE, type = 7, ... )
      q[, i] <-  cut( matrix[, i], breaks = xq, labels = FALSE, include.lowest = TRUE)

      #Let the first quantile be all the BDLs.
      q[which( is.na(q[, i]) ), i] <- 0
    }

    #Checking the method: show only if verbose = TRUE.
    if(verbose){
      cat("Summary of Quantiles \n")
      print( apply(q, 2, table) )
      cat("Total Number of NAs--Q1 (The first row) should match.\n")
      print( t( apply(matrix, 2, function(i) {sum(is.na(i)) } ) ) )
      # cat("\n Summaries: (a) Matrix \n")
      #  print(   apply(matrix, 2, my.summary ) )
      # cat("\n Summaries: (a) Quantiles \n")
      #  print(   apply(q, 2, my.summary ) )
    }
  }

  #Return the quantiles.
  return(q)
}


#Log: make.quantile.bdl
#Modified 8 /14 /17: Added error check to make it easier to read.
#Modified 8/ 22/17: Added conventional practice of placing all the BDLs in the first quantile.
#Modified 9/3/17: Add example and copied to WQS_Analysis#Add this parameter to general WQS function.
#place.bdls.in.Q1 #logical. If TRUE, uses the default techinque of placing the BDLs in the first quantile.
#Modified 10/16/18: Changed name to be unique and not related to S3 generic quantile. Added check to make sure that numeric is used.


