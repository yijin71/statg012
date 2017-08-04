#'Normal sampling distribution with a normal distribution of prior
#'
#'Define the posterior distribution function for \eqn{f( x | \mu, \sigma )}, with
#'a normal prior distribution \eqn{g( \mu, \sigma )} and a normal sampling
#'distribution \eqn{f( \mu, \sigma | x )}.
#'
#'@param x a vector of observations from a normal distribution with mu and
#'sigma.
#'@param \eqn{m},\eqn{s} parameters from a normal distribution of mu prior.
#'@param \eqn{\alpha},\eqn{\beta} parameters from a gamma distribution. If
#'mu and sigma is NULL, the prior distribution on \eqn{\mu} and precision
#'\eqn{\tau} has a Normal-Gamma distribution. \eqn{\tau = 1/\sigma^2}
#'@param mu the mean of x vector from a normal distribution. If it is NULL,
#'the mean of x is unknown.
#'@param sigma the standard deviation of x vector from a normal distribution.
#'If it is NULL, the standard deviation of x is unknown.
#'
#'@return An object of class "\code{g12post}" is returned.
#'
#'@examples
#'
#' ## this example is from slides6 Example 5.7
#' ## generate a sample of 9 from a normal distribution with sd=2
#' x <- rnorm(9, sd = 2)
#' ## the observed sample mean is 20
#' xx <- x- mean(x) + 20
#' ## find the posterior density
#' normnorm(xx, m = 25, s = sqrt(10), sigma = 2)
#'
#' ##
#' x <- rnorm(13, mean = 2, sd = 0.5)
#' a <- normnorm(x, m = 0, s = 1, alpha = 1, beta = 1)
#'@seealso The
#'\href{https://moodle.ucl.ac.uk/mod/folder/view.php?id=2570901}{slides}
#'of STATG012 on Moodle
#'
#'@export normnorm

normnorm <- function (x, m, s, alpha = NULL, beta = NULL, mu = NULL,
                     sigma = NULL) {

  model <- "normnorm"
  if (is.null(sigma)) {
      if (is.null(alpha) | is.null(beta)) {
      stop ("Since sigma is unknown, the precision follows a
            gamma distribution with parametr alpha and beta which
            should be known")
      } else {

        z <- stats::qgamma(0.9999, shape = alpha, rate = beta)
        tau <- seq(0,z, length.out = 1000)
        tau.prior <- stats::dgamma(tau, shape = alpha, rate = beta)
        muu <- m + 5
        mul <- m - 5
        mu <- seq(mul, muu, length.out = 1000)
        mu.prior <- (beta + s * 0.5 * (mu - m) ^ 2) ^ (- alpha - 0.5)

        prior <- tau ^ (alpha - 0.5) * exp(- beta * tau) *
          exp(-0.5 * tau * s * (mu - m) ^ 2)

        ######################################################################
        #likelihood x~N(mu, tao^-1)

        n <- length(x)
        x_mean <- mean(x)
        s_var <- 1 / n * sum((x - x_mean) ^ 2)
        ss <- n * s_var
        likelihood <- tau ^ (n / 2) * exp((-tau/2) *
                                            (n * s_var + n * (x_mean-mu) ^2))

        ######################################################################
        #posterior
        pos.s <- s + n
        pos.m <- (s * m + n * x_mean) / pos.s
        pos.alpha <- alpha + n/2
        pos.beta <- beta + 0.5 * (ss + (s * n * (x_mean - m)^2 /pos.s))

        tau.posterior <- stats::dgamma(tau, pos.alpha, pos.beta)

        mu.posterior <- stats::dt(mu, df = 2 * pos.alpha)

        pos1 <- pos.alpha - 0.5
        pos2 <- tau * pos.s * 0.5 * (mu - pos.m) ^ 2
        pos3 <- tau * (beta + ss * 0.5)
        pos4 <- 0.5 * tau * (s * (m ^ 2) + n * (x_mean ^2) + pos.s * (pos.m ^ 2))
        posterior <- tau ^ pos1 * exp(-pos2) * exp(-pos3) * exp(-pos4)

        res <- list( mu = mu,
                     tau = tau,
                     mu.prior = mu.prior,
                     tau.prior = tau.prior,
                     prior = prior,
                     likelihood = likelihood,
                     mu.posterior = mu.posterior,
                     tau.posterior = tau.posterior,
                     posterior = posterior,
                     pos.m =  pos.m,
                     pos.s =  pos.s,
                     pos.alpha = pos.alpha,
                     pos.beta = pos.beta,
                     model = model)

        cat(paste("Posterior mu       : ", round(pos.m,4),"\n",sep=""))
        cat(paste("Posterior s        : ", round(pos.s,4),"\n",sep=""))
        cat(paste("Posterior alpha    : ", round(pos.alpha,4),"\n",sep=""))
        cat(paste("Posterior beta     : ", round(pos.beta,4),"\n",sep=""))

         class(res) <- "g12post"
        invisible(res)
        }

   } else {

    if (is.null(mu)) {
     z <- stats::qnorm(c(0.0001,0.9999), m, s)
     mu <- seq(z[1], z[2], length.out = 1000)
     }
  prior <- stats::dnorm(mu, m, s)
  pri.precision <- 1 / s^2
  n <- length(x)
  x_mean <- mean(x)
  likelihood <- exp(-n / (2 * sigma ^ 2 ) * (x_mean- mu) ^ 2)

  pos.precision <- pri.precision + (n / sigma ^ 2)
  pos.var <- 1 / pos.precision
  pos.std <- sqrt(pos.var)
  pos.mean <- (pri.precision / pos.precision * m) + (
    (n / sigma ^ 2) / pos.precision * x_mean)

  posterior <- stats::dnorm(mu, pos.mean, pos.std)

  res <- list( mu = mu,
               prior = prior,
               likelihood = likelihood,
               posterior = posterior,
               pri.precision = pri.precision,
               pri.mean = m,
               pri.std = s,
               pos.precision= pos.precision,
               pos.mean = pos.mean,
               pos.std = pos.std,
               model = model)

  cat(paste("Posterior precision      : ",round(pos.precision,4),"\n",sep=""))
  cat(paste("Posterior mean           : ",round(pos.mean,4),"\n",sep=""))
  cat(paste("Posterior variance       : ",round(pos.var,4),"\n",sep=""))
  cat(paste("Posterior std. deviation : ",round(pos.sd,4),"\n",sep=""))

  class(res) <- "g12post"
  invisible(res)
   }
 }


