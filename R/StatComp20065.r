#' @title a naive implement of computing skewness using R.
#' @description a function from hw-10-27 return a sample's skewness
#' @param x a random sample vector (numeric)
#' @return a skewness of sample x
#' @examples
#' \dontrun{
#' x=rnorm(1000)
#' x.skew <- sk(x)
#' }
#' @export
sk = function(x) {
  xbar = mean(x)
  m3 = mean((x - xbar)^3)
  m2 = mean((x - xbar)^2)
  return( m3 / m2^1.5 )
}

#' @title Estimate the power of the skewness test of normality against symmetric Beta distributions.
#' @description a function from hw-10-27
#' @param a parameter vector for Beta(a,a) (numeric)
#' @return a list gives the power of skewness test of normality against Beta(a,a) distribution, and the standard error of the test.
#' @examples
#' \dontrun{
#' a = c(seq(0,1,0.1),seq(1,20,1),seq(20,100,10))
#' pwr = pwr_beta(a)$pwr
#' se = pwr_beta(a)$se
#' plot(a, pwr, type = "b", xlab = "a", ylab = "pwr", pch=16)
#' abline(h = 0.1, lty = 2)
#' lines(a, pwr+se, lty = 4)
#' lines(a, pwr-se, lty = 4)
#' }
#' @export
pwr_beta = function(a){
  alpha = 0.1
  n = 20
  m = 1e4
  N = length(a)
  pwr = numeric(N)
  cv = qnorm(1-alpha/2, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))
  
  for (j in 1:N) { 
    sktests = numeric(m)
    for (i in 1:m) { 
      x = rbeta(n, a[j], a[j])
      sktests[i] = as.integer(abs(sk(x))>= cv)
    }
    pwr[j] = mean(sktests)
  }
  se = sqrt(pwr * (1-pwr) / m) 
  return(list(pwr = pwr,se = se))
}