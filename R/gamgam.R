#' Gamma sampling distribution with a gamma distribution of prior
#'
#' Define the posterior distribution function for \eqn{\pi (\theta | t )},
#' with a gamma prior distribution \eqn{\pi ( \theta; \alpha, \lambda )} and a
#' gamma sampling distribution with known shape parameter a
#' \eqn{p (t; a, \theta)}.
#'
#'@param t a sample data from a gamma distribution with known shape a and rate
#'theta ( t > 0 )
#'@param a the known shape parameter of the gamma sampling distribution ( > 0)
#'@param shape the shape parameter for the gamma distribution of prior ( > 0).
#'@param rate  the rate parameter for the gamma distribution of prior( > 0).
#'@param scale  equals 1 / rate ( > 0).
#'@return An object of class "\code{g12post}" is returned.
#'\item{prior}{the prior distribution, i.e.
#' the \eqn{gamma(\alpha,\lambda} distribution.} \item{likelihood}{the likelihood
#' function of t given \eqn{\theta}, i.e. the
#' \eqn{gamma(a,\theta)} distribution.} \item{posterior}{the
#' posterior distribution of \eqn{\theta} given t.} \item{theta}{the
#' rate parameter of the gamma distribution and its prior is
#' a gamma distribution
#' \eqn{\pi ( \theta; \alpha, \lambda )}.} \item{pri.shape}{the
#' shape parameter for the gamma distribution of prior.} \item{pri.rate}{the
#' rate parameter for the gamma distribution of prior.} \item{pos.shape}{the
#' shape parameter for the gamma distribution of
#' posterior.} \item{pos.rate}{the rate parameter for the gamma distribution
#' of posterior.}\item{model}{the prior and likelihood type to produce the
#' posterior.}
#' @source For theory details, based on
#'\href{https://en.wikipedia.org/wiki/Conjugate_prior}{Wikipedia's article on
#'gamma conjugate prior} and STATG012 slides5 Example5.4 on Moodle at UCL.
#'\href{https://moodle.ucl.ac.uk/mod/folder/view.php?id=2570901}{STATG012
#' slides5}
#'@seealso \code{\link{summary.g12post}} for summararies of prior
#'and posterior distribution.
#'@seealso \code{\link{plot.g12post}} for plots of prior and posterior
#'distribution.
#'@examples
#'## an exponential distribution with a gamma prior, similiar
#'## as Example 5.4 from slides 6
#'## generate a sample of 10 observations from an exponential distribution
#'x <- rexp(10)
#'## find the posterior density and summary it
#'gam <- gamgam(x, a = 1, 4, 2)
#'summary(gam)
#'
#'## generate a sample of 50 observations from a gamma distribution with a = 2
#'y <- rgamma(10, shape = 2)
#'## find the posterior density and plot it
#'ex <- gamgam(y, a = 2, 4, 2)
#'plot(ex, leg_pos = "top", box.col = "n")
#'@export gamgam
#'
gamgam <- function(t, a = 1, shape, rate, scale = 1 / rate) {
  ############################################################################
  ## t : a sample data from a gamma distribution with a, theta (x > 0)
  ## shape, rate, scale  : parameters of gamma distribution ( > 0)

  n <- length(t)
  t_sum <- sum(t)
  s <- shape
  r <- rate
  scale <- 1 / r
  ############################################################################
  ## conditions

  if (any(t < 0)) {
    stop(" t should be larger than zero")
  }

  if (a <= 0) {
    stop("The shape parameter should be larger than zero")
  }
  if (s <= 0 | r <= 0) {
    stop("The shape and rate parameter should be larger than zero")
  }
  ############################################################################
  ## formula : slides 6 (ex.5.4) + reference
  z <- stats::qgamma(0.9999, s, r)
  theta <- seq(0, z, z/1000)
  #
  prior <- stats::dgamma(theta, s, r)

  likelihood <- matrix(0, ncol = length(theta), nrow = n )
  for(i in 1:length(theta)) {
    likelihood[,i] = stats::dgamma(t, a, theta[i])
  }
  likelihood <- apply(likelihood, 2, prod)

  pos.s <- s + a * n
  pos.r <- r + t_sum
  posterior <- stats::dgamma(theta, pos.s, pos.r)
  model <- "gamgam"
  #############################################################################
  res <- list(    prior = prior,
                  likelihood = likelihood,
                  posterior = posterior,
                  theta = theta,
                  pri.shape = s,
                  pri.rate = r,
                  pos.shape = pos.s,
                  pos.rate = pos.r,
                  model = model)

  cat(paste("Posterior shape    : ",round(pos.s,4),"\n",sep=""))
  cat(paste("Posterior rate     : ",round(pos.r,4),"\n",sep=""))


  class(res) <- "g12post"
  invisible(res)
}
