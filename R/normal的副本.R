#'Normal sampling distribution with a normal distribution of prior
#'
#'Define the posterior distribution function for \eqn{f( x | \mu, \sigma )}, with
#'a normal prior distribution \eqn{g( \mu, \sigma )} and a normal sampling
#'distribution \eqn{f( \mu, \sigma | x )}.
#'
#'@param x a vector of observations from a normal distribution.
#'@param mu_m the mean of a normal distribution of prior.
#'@param mu_sd the standard deviation of a normal distribution of prior.
#'@param x_mu the mean of x vector from a normal distribution. If it is NULL,
#'the mean of x is unknown.
#'@param x_sd the standard deviation of x vector from a normal distribution. If
#'it is NULL, the standard deviation of x is unknown.
#'
#'@return An object of class "\code{g12post}" is returned.
#'\item{prior}{the prior distribution, i.e.
#' the \eqn{g(\mu)} distribution, if standard deviation is known}
#' \item{likelihood}{the likelihood function of x given \eqn{\mu, \sigma}}
#' \item{posterior}{the posterior distribution of \eqn{\mu, \sigma}
#' given x} \item{pos.mean}{the posterior mean} \item{pos.var}{the
#' posterior variance} \item{pos.sd}{the posterior standard deviation}
#'
#' @examples
#' ## this example is from slides6 Example 5.7
#' ## generate a sample of 9 from a normal distribution with sd=2
#' x <- rnorm(9, sd = 2)
#'
#' ## the observed sample mean is 20
#' xx <- x- mean(x) + 20
#'
#' ## find the posterior density
#' normal(xx, mu_m = 25, mu_sd = sqrt(10), x_sd = 2)
#'
#'@export normal
normal <- function(x, mu_m, mu_sd, x_mu = NULL, x_sd = NULL) {

  x_mean <- mean(x)
  x_n <- length(x)
###############################################################################
# standard deviation of x
  if (is.null(x_sd)) {
    x_sd <- 1 / (x_n-1) * sum((x - x_mean) ^ 2)
  } else {
    if (x_sd < 0) {
      stop("Standard deviation x_sd must be greater than zero")
    }
  }
###############################################################################
# mean of x
  if(is.null(x_mu)) {
    lb <- 0
    ub <- 0

    if(mu_sd == 0) {
      lb <-  x_mean - 1.96 * x_sd / sqrt(x_n)
      ub <-  x_mean + 1.96 * x_sd / sqrt(x_n)
    } else {
      lb <- mu_m - 1.96 * mu_sd
      ub <- mu_m + 1.96 * mu_sd
    }
    x_mu <- seq(lb, ub)

  } else {
    x_mu <- seq(min(x_mu), max(x_mu))
  }

#####################################################################
# prior distribution
  if(mu_sd == 0) {
    prior.precision <- 0
    mu_m <- 0
    lb <- min(x_mu)
    ub <- max(x_mu)
    prior <- rep(1 / (ub - lb), length(x_mu))
  } else {
    prior <- stats::dnorm(x_mu, mu_m, mu_sd)
    pri.precision <- 1 / (mu_sd) ^ 2
  }

###############################################################################
#likelihood
likelihood <- exp(-x_n /(2 * x_sd ^ 2) * (x_mean - x_mu) ^ 2)

###############################################################################
#posterior

pos.precision <- pri.precision + (x_n / x_sd ^ 2)
pos.var <- 1 / pos.precision
pos.sd <- sqrt(pos.var)
pos.mean <- (pri.precision / pos.precision * mu_m) + (
             (x_n / x_sd ^ 2) / pos.precision * x_mean)

posterior <- stats::dnorm(mu_m, pos.mean, pos.sd)
###########################################################################

res <- list( prior = prior,
             likelihood = likelihood,
             posterior = posterior,
             pos.precision = pos.precision,
             pos.mean = pos.mean,
             pos.var = pos.var,
             pos.sd = pos.sd)

cat(paste("Posterior precision      : ",round(pos.precision,4),"\n",sep=""))
cat(paste("Posterior mean           : ",round(pos.mean,4),"\n",sep=""))
cat(paste("Posterior variance       : ",round(pos.var,4),"\n",sep=""))
cat(paste("Posterior std. deviation : ",round(pos.sd,4),"\n",sep=""))

class(res) <- "g12post"
invisible(res)
}
