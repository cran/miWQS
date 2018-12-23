#  In makeJournalTables: Don't export.
#  #' Expanded Summary Function
#  #'
#  #'@family statistics
#  #'
#  #'@description
#  #'Takes any numeric vector x and expands what is provided by summary with additional statistics:
#  #'number of subjects, number of missing values, and standards deviation.
#  #'
#  #'@note
#  #' Statistics ignore the missing values. This function is useful when combined with apply or aggregate.
#  #'
# Since aggregate() drops the names of the statistics, \code{aggregate.my.summary} function aggregates
# the data using \code{my.summary} and returns a data frame with the names of statistics. The arguments of my.summary is
# passed to aggregate.my.summary(). Missing values are kept. See \code{\link[stats]{aggregate}} (class: formula).
#  #'
#  #'
#  #'@param x  A numeric vector.
#  #'@param sig.digits  The number of significant digits shown in a result from my.summary. Default: 6.
#  #'@param na.rm Logical: whether to remove missing values. Default: TRUE.
#  #'@return A matrix of sample size(n), minimum (min), first quartile (FrstQ), median, mean, third quartile (ThrdQ),
#  #'        max, standard deviation (sd), and number of missing values (NA.s)
#  #'
#  #'@examples
#  #' #Example 1: Compare base::summary to my.summary
#  #' base::summary(CO2$conc)
#  #' my.summary(CO2$conc)
#  #'
#  #' #Used with aggregate or apply.
  #aggregate.my.summary(conc~Treatment, data=CO2,4)
#  #' aggregate(conc~Treatment, data=CO2, my.summary, 4)
#  #' apply(CO2[,4:5], 2, my.summary)

#  #' #Example 2: Another dataset not CO2
#  #' iris <- datasets::iris
#  #' summary(iris$Sepal.Length)
#  #' my.summary( iris$Sepal.Length )

#aggregate.my.summary( Sepal.Length ~ Species , data = iris )
# more complex
# l <- list(CO2[CO2$Type=="Quebec",],
#         CO2[CO2$Type=="Mississippi",])
# lapply(l, my.summary) #summarize.unique)

my.summary<-function(x, sig.digits=6, na.rm=TRUE){
    if(is.vector(x) || is.factor(x)){
      n<- length(x)
    } else {
      n<-nrow(x)
    }

    if(n>0){
      A<-  matrix ( c(n=n,
                      min= signif( min(x,na.rm=na.rm) ,sig.digits),
                      FrstQ= signif( quantile(x,0.25,na.rm,names=FALSE) ,sig.digits),
                      median= signif( median(x,na.rm) ,sig.digits),
                      mean= signif( mean(x,na.rm = na.rm) ,sig.digits) ,
                      ThrdQ= signif( quantile(x,0.75,na.rm=na.rm,names=FALSE) ,sig.digits),
                      max=signif( max(x,na.rm=na.rm) ,sig.digits),
                      sd=signif( sd(x,na.rm = na.rm),sig.digits),
                      NA.s= sum(is.na(x))
      ),nrow=1,ncol=9, dimnames=list(NULL, c( "n" , "min" ,"FrstQ" ,"median", "mean" , "ThrdQ", "max", "sd", "NA.s")))

    } else { if(n==0){  #The vector is empty
      A<-matrix(c(0,rep(NA,7),n),nrow=1,ncol=9, dimnames=list(NULL, c( "n" , "min" ,"FrstQ" ,"median", "mean" , "ThrdQ", "max", "sd", "NA.s")))
    }}

    return(A)
    #return(list(A=A, names.summary=names.summary))
}

# #  #'Uses the my.summary function and aggregates the data.
# #  #'@rdname my.summary
# #  #'@inheritParams stats::aggregate
# #  #'@import datasets
# #  #'@param ...	 Additional parameters passed to my.summary, but is defunct and not used.
# #  #'@export aggregate.my.summary

aggregate.my.summary<-function(formula, data, sig.digits=6, subset){

  #Take the names from my.summary from CO2 dataset
  CO2 <- datasets::CO2
  my.summary.names <- colnames(my.summary(CO2$conc))

  #Aggregate using function my.summary
  x <- aggregate(formula, data, my.summary, sig.digits, subset = subset,
               na.action=na.pass)

  #Save the names on aggregate object x.
  colnames(x[,2]) <- my.summary.names

  #Return
  return(x)
}


