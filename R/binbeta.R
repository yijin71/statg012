#' Binomial sampling distribution with a beta distribution of prior
#'
#'Define the posterior distribution function for \eqn{\pi ( \theta | r )}, with
#'a beta prior distribution \eqn{\pi ( \theta; a, b )} and a binomial sampling
#'distribution \eqn{p ( r | \theta )}.
#'
#'@param n the number of trials in binomial distribution.
#'@param r the number of successes in n trials.
#'@param alpha parameter for the beta distribution ( \eqn{\ge 0} ).
#'@param beta parameter for the beta distribution ( \eqn{\ge 0} ).
#'@param \eqn{\theta} the range of the probability of success.
#'@return An object of class "\code{binbeta}" is returned.
#'\item{prior}{the prior distribution, i.e.
#' the \eqn{beta(\alpha,\beta)} distribution} \item{likelihood}{the likelihood
#' function of r given \eqn{\theta}, i.e. the
#' \eqn{binomial(n,r)} distribution} \item{posterior}{the
#' posterior distribution of \eqn{\theta} given r, i.e. the
#' \eqn{beta(\alpha+r, \beta+n-r)} distribution} \item{theta}{the
#' range of the probability of success} \item{pri.alpha}{the
#' alpha parameter for the beta distribution of prior} \item{pri.beta}{the
#' beta parameter for the beta distribution of prior} \item{pos.alpha}{the
#' alpha parameter for the beta distribution of posterior} \item{pos.beta}{the
#' beta parameter for the beta distribution of posterior}
#'@seealso \code{\link{summary.binbeta}} will return summararies of prior
#'and posterior distribution.
#'@examples
#'## simplest one with 3 successes in 10 trials and a uniform beta(1,1) prior
#'binbeta(n = 6, r = 2)
#'
#'## example 3.6 : 3 successes in 10 trials and a beta(4,6) prior
#'binbeta(4, 6, 10, 3)
#'
#'@export

  binbeta <- function(alpha = 1, beta = 1, n, r, theta = seq(0,1,0.001)) {
  #############################################################################
  #n: no. of  binomial trials
  #r: no. of successes in n trials (0<= r<= n)
  #alpha, beta: are parameters of beta distribution of prior ( >0)
  #theta: the range of the probability of success

  if (n < 0) {
   stop("The number of trials n should not smaller than zero")
  }

  if (n < r) {
    stop("The number of successes r should not larger than trials n")
  }

  if (alpha <= 0 | beta <= 0) {
    stop("The parameters of beta distribution should be larger than zero")
  }
  #############################################################################
  # formula: slides 2 example 3.6 (p.45)

  prior <- stats::dbeta(theta, alpha, beta, log = FALSE)
  likelihood <- stats::dbinom(r, n, prob = theta, log = FALSE)
  pos.alpha <- alpha + r
  pos.beta <- beta + n-r
  posterior <- stats::dbeta(theta, pos.alpha, pos.beta)

 ##############################################################################
 # results
  res <- list( prior = prior,
               likelihood = likelihood,
               posterior = posterior,
               theta = theta,
               pri.alpha = alpha,
               pri.beta = beta,
               pos.alpha = pos.alpha,
               pos.beta = pos.beta)

  class(res) <- "g12post"
  invisible(res)
  }


