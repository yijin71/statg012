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
#'@seealso The
#'\href{https://moodle.ucl.ac.uk/mod/folder/view.php?id=2570901}{slides}
#'of STATG012 on Moodle
#'
#'@export normnorm

normnorm <- function (x, m, s, alpha = NULL, beta = NULL, mu = NULL,
                     sigma = NULL) {
  if (is.null(sigma)) {
      if (is.null(alpha) | is.null(beta)) {
      stop ("Since sigma is unknown, the precision follows a
            gamma distribution with parametr alpha and beta which
            should be known")
      } else {

#####################################################################
#prior
        z <- stats::qnorm(0.9999, m, s)
        mu <- seq(0, z, z/1000)
        mu.prior <- stats::dnorm(mu, m, s)
        zz<- stats::qgamma(0.9999, alpha, beta)
        tau <- seq(0, zz, zz/length(mu))
        tau <- tau[1:length(tau) - 1]
        tau.prior <- stats::dgamma(tau, alpha, beta)
        prior <- tau^(alpha - 0.5) * exp(- beta * tau) *
                 exp(-0.5 * s * tau * (mu - m)^2)
######################################################################
#likelihood x~N(mu, tao^-1)
        n <- length(x)
        x_mean <- mean(x)
        s_var <- 1/n * sum((x - x_mean)^2)
        likelihood <- tau^(n/2) * exp((-tau/2) *
                                      (n * s_var + n * (x_mean-mu) ^2))

######################################################################
#posterior
        pos.m <- (s * m + n * x_mean)/(s + n)
        pos.s <- s + n
        pos.alpha <- alpha + n/2
        pos.beta <- beta + 0.5 * (n * s_var +
                                  (s * n * (x_mean - m)^2 /pos.s))

        mu.pos <- stats::dnorm(mu, pos.m, pos.s)
        tau.pos <- stats::dgamma(tau, pos.alpha, pos.beta)


        posterior <- tau.pos ^(pos.alpha - 0.5) * exp(-tau.pos * pos.beta) *
          exp(-tau.pos/2 * pos.s * (mu.pos - pos.beta)^2)

        res <- list( mu = mu,
                     tau = tau,
                     mu.prior = mu.prior,
                     tau.prior = tau.prior,
                     prior = prior,
                     likelihood = likelihood,
                     mu.pos = mu.pos,
                     tau.pos = tau.pos,
                     posterior = posterior,
                     pos.m =  pos.m,
                     pos.s =  pos.s,
                     pos.alpha = pos.alpha,
                     pos.beta = pos.beta)

        cat(paste("Posterior mu0      : ", round(pos.m,4),"\n",sep=""))
        cat(paste("Posterior lamda0   : ", round(pos.s,4),"\n",sep=""))
        cat(paste("Posterior alpha    : ", round(pos.alpha,4),"\n",sep=""))
        cat(paste("Posterior beta     : ", round(pos.beta,4),"\n",sep=""))

        class(res) <- "g12post"
        invisible(res)
      }

   } else {

    if (is.null(mu)) {
     z <- stats::qnorm(0.9999, m, s)
     mu <- seq(0, z, z/1000)
     }

  prior <- stats::dnorm(mu, m, s)
  pri.precision <- 1 / s^2
  n <- length(x)
  x_mean <- mean(x)
  likelihood <- exp(-n / (2 * sigma ^ 2 ) * (x_mean- mu) ^ 2)

  pos.precision <- pri.precision + (n / sigma ^ 2)
  pos.var <- 1 / pos.precision
  pos.sd <- sqrt(pos.var)
  pos.mean <- (pri.precision / pos.precision * m) + (
    (n / sigma ^ 2) / pos.precision * x_mean)

  posterior <- stats::dnorm(mu, pos.mean, pos.sd)

  res <- list( mu = mu,
               prior = prior,
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
 }


