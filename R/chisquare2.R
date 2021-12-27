#' Chi Square statistics - Two Way Table
#'
#' Returns a list with the the chi-square test statistic, right-sided
#' p-value and degrees of freedom for a given matrix.
#'
#' Input is a matrix of values.
#'
#'@export
chisquare2 <- function(m,x2=0){
  if(is.matrix(m)){
    for (i in 1:nrow(m)){
      for (j in 1:ncol(m)){
        ev <- sum(m[i,]) * sum(m[,j]) / sum(m)
        x2 <- x2 + ((m[i,j] - ev)^2)/ev
      }
    }
    df <- (nrow(m)-1) * (ncol(m)-1)
    pval <- pchisq(x2,df,lower.tail=FALSE)
    return(list(pval=pval,x2=x2,df=df))
  }
  else{
    stop('input is not a matrix')
  }
}
