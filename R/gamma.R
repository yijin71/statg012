#' Gamma sampling distribution with a gamma distribution of prior
#'
#' Define the posterior distribution function for \eqn{\pi (\theta | t )},
#' with a gamma prior distribution \eqn{\pi ( \thta; \alpha, \lambda )} and a
#' gamma sampling distribution \eqn{p (t; a, \theta)}.
#'
#'@param t a sample data from a  gamma distribution with shape a and rate
#'theta ( t > 0 )
#'@param a the shape of the gamma distribution ( > 0)
#'@param shape the shape parameter for the gamma distribution of prior ( > 0).
#'@param rate  the rate parameter for the gamma distribution of prior( > 0).
#'@param scale  equals 1 / rate ( > 0).
#'
#'@return An object of class "\code{g12post}" is returned.
#'
#'@examples
#'
#'## an exponential distribution with a gamma prior, similiar
#'## as Example 5.4 from slides 6
#'## generate a sample of 10 observations from an exponential distribution
#'x <- rexp(10)
#'
#'## find the posterior density
#'gamma(x, a = 1, 4, 2)
#'
#'## generate a sample of 50 observations from a gamma distribution with a = 2
#'y <- rgamma(50, shape = 2)
#'
#'## find the posterior density
#'gamma(y, a = 2, 4, 2)
#'
#'@seealso The
#'\href{https://moodle.ucl.ac.uk/mod/folder/view.php?id=2570901}{slides}
#'of STATG012 on Moodle
#'
#'@export gamma
gamma <- function(t, a = 1, shape, rate, scale = 1 / rate) {
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
    stop("The shape should be larger than zero")
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
  pos.mean <- pos.s / pos.r
  pos.var <- pos.s / pos.r^2
  #############################################################################
  res <- list(    theta = theta,
                  prior = prior,
                  likelihood = likelihood,
                  posterior = posterior,
                  pos.s = pos.s,
                  pos.r = pos.r,
                  pos.mean = pos.mean,
                  pos.var = pos.var)

  cat(paste("Posterior shape    : ",round(pos.s,4),"\n",sep=""))
  cat(paste("Posterior rate     : ",round(pos.r,4),"\n",sep=""))
  cat(paste("Posterior mean     : ",round(pos.mean,4),"\n",sep=""))
  cat(paste("Posterior variance : ",round(pos.var,4),"\n",sep=""))

  class(res) <- "g12post"
  invisible(res)
}
