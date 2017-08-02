#' Poisson sampling distribution with a gamma distribution of prior
#'
#'Define the posterior distribution function for \eqn{\pi ( \mu | x )}, with
#'a gamma prior distribution \eqn{\pi ( \mu; \alpha, \lambda )} and a
#'poisson sampling distribution \eqn{f (x | \mu)}.
#'
#'@param x the sample data from poisson distribution  (x > 0).
#'@param shape the shape parameter for a gamma distribution (> 0).
#'@param rate  the rate parameter for a gamma distribution (> 0).
#'@param scale  equals 1 / rate (> 0).
#'@return An object of class "\code{g12post}" is returned.
#'\item{prior}{the prior distribution, i.e.
#' the \eqn{gamma(\alpha,\lambda)} distribution.} \item{likelihood}{the
#' likelihood function of x given \eqn{\mu}, i.e. the
#' \eqn{Poisson( x | \mu)} distribution.} \item{posterior}{the
#' posterior distribution of \eqn{\mu} given x.} \item{mu}{the expected number
#' of occurence which is the parameter of poisson
#' distribution.} \item{pri.shape}{the shape parameter for the gamma
#' distribution of prior.} \item{pri.rate}{the rate parameter for
#' the gamma distribution of prior.} \item{pos.shape}{the shape parameter for the gamma
#' distribution of posterior.} \item{pos.rate}{the
#' rate parameter for the gamma distribution of posterior.}\item{model}{the
#' prior and likelihood type to produce the posterior.}
#'
#'@source For theory details, based on
#'\href{http://people.stat.sc.edu/Hitchcock/slides535day5spr2014.pdf}{The
#'Gamma/Poisson Bayesian Model} and STATG012 slides5 Example5.6 on Moodle
#'at UCL.
#'\href{https://moodle.ucl.ac.uk/mod/folder/view.php?id=2570901}{STATG012
#' slides5}
#'@seealso \code{\link{summary.g12post}} for summararies of prior
#'and posterior distribution.
#'@seealso \code{\link{plot.g12post}} for plots of prior and posterior
#'distribution.
#'
#'@examples
#'## an observation of 10 with an exponential prior
#'a <- poisgamma(10, 1, 1)
#'summary(a)
#'
#'## An Example 5.6 from slides 6, summary and plot it
#'x <- c(2, 10)
#'ex <- poisgamma(x, 4, 1)
#'summary(ex)
#'plot(ex,leg_pos = "right",cex = 1, lty = 5:6,
#'main = "Distributions of Example5.6")

#'
#'@export poisgamma

poisgamma <- function(x, shape, rate, scale = 1 / rate) {
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
  model <- "poisgamma"
#############################################################################
  res <- list(    prior = prior,
                  likelihood = likelihood,
                  posterior = posterior,
                  mu = mu,
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








