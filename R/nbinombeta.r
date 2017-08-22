#' Negative binomial sampling distribution with a beta distribution of prior
#'
#'Define the posterior distribution function for \eqn{\pi ( \theta | k )}, with
#'a beta prior distribution \eqn{\pi ( \theta; \alpha, \beta )} and a negative
#'binomial sampling distribution \eqn{p ( k | \theta )}.
#'
#'@param k the number of failures in rth successes
#'@param r the rth successes.
#'@param alpha the parameter for the beta distribution ( \eqn{\ge 0} ).
#'@param beta the parameter for the beta distribution ( \eqn{\ge 0} ).
#'@param theta the range of the probability of success.
#'@return An object of class "\code{g12post}" is returned.
#'\item{prior}{the prior distribution, i.e.
#' the \eqn{beta(\alpha,\beta)} distribution.} \item{likelihood}{the likelihood
#' function of k given \eqn{\theta}, i.e. the
#' \eqn{nbinomial(r,\theta)} distribution.} \item{posterior}{the
#' posterior distribution of \eqn{\theta} given k, i.e. the
#' \eqn{beta(\alpha+r, \beta+k)} distribution.} \item{theta}{the
#' range of the probability of success.} \item{pri.alpha}{the
#' alpha parameter for the beta distribution of prior.} \item{pri.beta}{the
#' beta parameter for the beta distribution of prior.} \item{pos.alpha}{the
#' alpha parameter for the beta distribution of posterior.} \item{pos.beta}{the
#' beta parameter for the beta distribution of posterior.}\item{model}{the
#' prior and likelihood type to produce the posterior.}
#' @source For the theory, based on
#' \preformatted{The STATG012 slides2 Example3.6 on Moodle at UCL.}
#' \href{https://moodle.ucl.ac.uk/mod/folder/view.php?id=2570901}{STATG012
#' slides2}
#' @references
#' Cook, JD. 2009. Notes on the Negative Binomial Distribution.
#' Available from:
#' \href{https://www.johndcook.com//negative_binomial.pdf}{reference link}.
#'@seealso \code{\link{summary.g12post}} for summararies of prior
#'and posterior distribution.
#'@seealso \code{\link{plot.g12post}} for plots of prior and posterior
#'distribution.
#'@examples
#'## example : 3 failures before the 2th successes and a beta(4,6) prior.
#'ex <- nbinombeta(4, 6, 3, 2)
#'## summary and plot the above example
#'summary(ex)
#'plot(ex,lty = 2:3, col = c(5,2))
#'@export nbinombeta

nbinombeta <- function(alpha = 1, beta = 1, k, r, theta = seq(0, 1,0.001)) {
  #############################################################################

  if (k < 0 | r < 0) {
    stop("The number of failures k and the rth successes
         should not be smaller than zero")
  }

  if (alpha <= 0 | beta <= 0) {
    stop("The parameters of beta distribution should be larger than zero")
  }
  #############################################################################
  prior <- stats::dbeta(theta, alpha, beta, log = FALSE)
  likelihood <- stats::dnbinom(k, r, prob = theta, log = FALSE)
  pos.alpha <- alpha + r
  pos.beta <- beta + k
  posterior <- stats::dbeta(theta, pos.alpha, pos.beta)
  model <- "nbinombeta"
  ##############################################################################
  # results
  res <- list( prior = prior,
               likelihood = likelihood,
               posterior = posterior,
               theta = theta,
               pri.alpha = alpha,
               pri.beta = beta,
               pos.alpha = pos.alpha,
               pos.beta = pos.beta,
               model = model)

  cat(paste("posterior alpha : ",round(pos.alpha, 5), "\n"))
  cat(paste("posterior beta  : ",round(pos.beta, 5), "\n"))

  class(res) <- "g12post"
  invisible(res)
}


