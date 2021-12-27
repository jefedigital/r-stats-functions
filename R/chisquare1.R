#' Chi Square statistics - One Way Table
#'
#' Returns a list with the the chi-square test statistic, right-sided
#' p-value and degrees of freedom for a given one-way table.
#'
#' Input is two vectors of observed and expected values.
#'
#' @export
chisquare1 <- function(obs,exp,x2=0){
  if (length(obs) == length(exp)){
    for (i in seq_along(obs)){
      x2 <- x2 + ((obs[i] - exp[i])^2)/exp[i]
    }
    df <- length(obs) - 1
    p <- pchisq(x2,df,lower.tail=FALSE)
    return(list(pval=p,x2=x2,df=df))
  }
  else {
    stop('length of the vectors do not match')
  }
}
