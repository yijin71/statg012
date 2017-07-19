#' Poisson sampling distribution with a gamma distribution of prior
#'
#'Define the posterior distribution function for \eqn{\pi ( \mu | x )}, with
#'a gamma prior distribution \eqn{\pi ( \mu; \alpha, \lambda )} and a
#'poisson sampling distribution \eqn{f(x; \mu)}.
#'
#'@param x the sample data from poisson distribution  (x > 0).
#'@param shape shape parameter for the gamma distribution (> 0).
#'@param rate  rate parameter for the gamma distribution (> 0).
#'@param scale  equals 1 / rate (> 0).
#'@return An object of class "\code{g12post}" is returned.
#'
#'@examples
#'## an observation of 10 and an exponential prior
#'poigamma(10, 1, 1)
#'
#'## Example 5.6 from slides 6
#'x <- c(2, 10)
#'poigamma(x, 4, 1)
#'
#'@seealso The
#'\href{https://moodle.ucl.ac.uk/mod/folder/view.php?id=2570901}{slides}
#'of STATG012 on Moodle
#'
#'@export poigamma

poigamma <- function(x, shape, rate, scale = 1 / rate) {
############################################################################
## x : a sample data from poisson distribution (x > 0)
## shape, rate, scale  : parameters of gamma distribution ( > 0)

  n <- length(x)
  x_sum <- sum(x)
  s <- shape
  r <- rate
  scale <- 1 / r
############################################################################
## conditions

  if (any(x < 0)) {
    stop(" x should be larger than zero")
  }

  if (s <= 0 | r <= 0) {
     stop("The shape and rate parameter should be larger than zero")
   }
############################################################################
## formula : slides6 (ex.5.6) + reference
  z <- stats::qgamma(0.9999, s, r)
  mu <- seq(0, z, z/1000)
  #
  prior <- stats::dgamma(mu, s, r)

  likelihood <- matrix(0, ncol = length(mu), nrow = n )
  for(i in 1:length(mu)) {
    likelihood[,i] = stats::dpois(x, mu[i])
  }
  likelihood <- apply(likelihood, 2, prod)

  pos.s <- s + x_sum
  pos.r <- r + n
  posterior <- stats::dgamma(mu, pos.s, pos.r)
  pos.mean <- pos.s / pos.r
  pos.var <- pos.s / pos.r^2
#############################################################################
  res <- list(    mu = mu,
                  prior = prior,
                  likelihood = likelihood,
                  posterior = posterior,
                  pos.s = pos.s,
                  pos.r = pos.r,
                  pos.mean = pos.mean,
                  pos.var = pos.var
                  )
  cat(paste("Posterior shape    : ",round(pos.s,4),"\n",sep=""))
  cat(paste("Posterior rate     : ",round(pos.r,4),"\n",sep=""))
  cat(paste("Posterior mean     : ",round(pos.mean,4),"\n",sep=""))
  cat(paste("Posterior variance : ",round(pos.var,4),"\n",sep=""))

  class(res) <- "g12post"
  invisible(res)
}








