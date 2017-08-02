#'Creat the posterior distribution
#'
#'@param a paramter of the prior distribution
#'@param b paramter of the prior distribution
#'
#'@export
posterior  <- function(a, b, c, d,..., n = 4, model = ""){

  if (n > 4 & model == "binombeta") {
    res <- binbeta(alpha = a, beta = b, n = c, r = d, theta = seq(0, ..., 0.01))
  }
  if (n > 4 & model == "gamma") {
    res <- gamma(t = a, a =b, shape = c, rate = d, scale = ...)
  }
  if (n > 4 & model == "normal") {
    res <-  normal(x = a, m = b , s = c, alpha = d, beta = ... )
  }

  if (n == 4 & model == "binombeta") {
    res <- binbeta(alpha = a, beta = b, n = c, r = d)
  }

  if (n == 4 & model == "poigamma") {
    res <- poigamma(x = a, shape = b, rate = c, scale = d)
  }

  if (n == 4 & model == "gamma") {
    res <- gamma(t = a, a =b, shape = c, rate = d)
  }

  if(n == 4 & model == "normal") {
    res <-  normal(x = a, m = b , s = c,  sigma = d )
  }
  return(res)
}


