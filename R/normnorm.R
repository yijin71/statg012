#'Normal sampling distribution
#'
#'Define the posterior distribution function for \eqn{\pi ( \mu |x )} if we have
#'a normal sampling distribution \eqn{f( x | \mu, \sigma^2 )} where \eqn{\mu}
#'is unknown and \eqn{\sigma^2} is known, and the normal prior distribution
#'\eqn{\pi ( \mu; m, s )} for unknown \eqn{\mu}. Define the posterior
#'distirbution \eqn{\pi ( \mu,\tau |x )} if we have a normal sampling
#'distribution where both \eqn{\mu} and \eqn{\sigma^2} are unknown, and the
#'normal-gamma prior distribution \eqn{\pi( \mu,\tau; m,s,\alpha,\beta )} for
#'unknown mean and precison \eqn{\tau} = \eqn{1/\sigma^2}.
#'
#'@param x a vector of observations from a normal distribution with mean
#'mu and variance sigma^2.
#'@param m,s two parameters of the prior normal distribution.
#'If mu is NULL, they are mean and standard deviation of prior
#'normal distribution for mu. If both mu and sigma are NULL, they are
#'mean and standard deviation of normal distribution
#'\eqn{\pi (\mu | \tau)}.
#'@param alpha,beta two parameters of a gamma distribution.
#'Only used when mu and sigma are both NULL, then the prior distribution
#'on \eqn{\mu} and precision \eqn{\tau} has a Normal-Gamma distribution.
#'The distribution of \eqn{\tau} is \eqn{gamma(\tau; \alpha, \beta)}
#'@param mu the mean of x vector from a normal distribution. If it is NULL,
#'the mean of x is unknown.
#'@param sigma the standard deviation of x vector from a normal distribution.
#'If it is NULL, the standard deviation of x is unknown.
#'
#'@return An object of class "\code{g12post}" is returned.
#'\item{prior}{the prior distribution.} \item{likelihood}{the likelihood
#' function.} \item{posterior}{the posterior distribution.} \item{mu}{the mean
#' of the x vector from a normal distribution.} \item{pri.mean}{the mean of the
#' prior distribution.} \item{pri.std}{the standard deviation of the prior
#' distribution.} \item{pri.precision}{the
#' precision of the prior distribution.} \item{pos.mean}{the mean of the
#' posterior distribution.} \item{pos.std}{the standard deviation of the
#' posterior distribution.} \item{pos.precision}{the
#' precision of the posterior distribution.}
#' \item{model}{the prior and likelihood type to produce the posterior.}
#'If both both mu and sigma are NULL, the addtional returns will be
#'\item{tau}{the precision of the x vector from a normal distribution.}
#'\item{mu.prior}{the prior distribution for mu.} \item{tau.prior}{the
#'prior distribution for tau.} \item{mu.posterior}{the
#'posterior distribution for mu.} \item{tau.posterior}{the
#'posterior distribution for tau.} \item{pos.m}{the parameter m in posteior
#'normal-gamma distribution.} \item{pos.s}{the parameter s in posteior
#'normal-gamma distribution.} \item{pos.alpha}{the parameter \eqn{\alpha} in
#'posteior normal-gamma distribution.} \item{pos.beta}{the parameter
#'\eqn{\beta} in posteior normal-gamma distribution.}
#'Notice \code{pri.mean}, \code{pri.std}, \code{pos.mean} and \code{pos.std}
#'will not return in this situation.
#'
#'@details Suppose we have a random sample x from normal distribution
#'N(\eqn{\mu},\eqn{\sigma^2}) whose \eqn{\mu}
#'is assumed to be unknow, the likelihood of this is \eqn{f (x | \mu)} and
#'the conjugate prior for this unknown \eqn{\mu} is a normal
#'distributin \eqn{\pi (\mu; m, s )}. Then the
#'posterior distribution \eqn{\pi (\mu | x )}, proportional to
#'\eqn{f (x | \mu)}\eqn{\pi (\mu; m, s )}, is also a normal distribution
#'N(\eqn{\mu_n}, \eqn{\sigma_n^2}).
#'
#'Suppose we have a random sample x from normal distribution
#'N(\eqn{\mu},\eqn{\sigma^2}) whose \eqn{\mu} and \eqn{\sigma^2} both are
#'assumed to be unknow, the likelihood of this is \eqn{f (x | \mu, \sigma^2)}.
#'The pricision \eqn{\tau} equals 1/\eqn{\sigma^2}, then the conjugate
#'prior for unknown \eqn{\mu} and \eqn{\tau} is a normal-gamma distribution
#'where \eqn{NG(\mu, \tau; m, s, \alpha,\beta)= N(\mu | \tau ; m, (s\tau)^-1)
#'G(\tau; \alpha,\beta)}. The posterior distribution is normal-gamma with
#'four updated parameters \eqn{m_n, s_n, \alpha_n, \beta_n}. In addtion, the
#'marginal distribution of \eqn{\mu} is student t distribution.
#'
#'@references
#'Hoff. 2010.Conjugate Priors for Normal Data, PowerPoint presentation,
#'STA290: Bayesian and Modern Data Analysis, Duke University.
#'Available from:
#'\href{https://www2.stat.duke.edu/courses/Fall10/sta290/Lectures/Normal/normal-conjugate.pdf}{Weblink}.
#'
#'Murphy, KP. 2007. Conjugate Bayesian analysis of the Gaussian distribution.
#'Available from:
#'\href{https://www.cs.ubc.ca/~murphyk/Papers/bayesGauss.pdf}{Weblink}.
#'
#'@source The
#'\href{https://moodle.ucl.ac.uk/mod/folder/view.php?id=2570901}{slides 6}
#'of STATG012 on Moodle
#'@seealso \code{\link{summary.g12post}} for summararies of prior
#'and posterior distribution.
#'@seealso \code{\link{plot.g12post}} for plots of prior and posterior
#'distribution.
#'@examples
#' ## this example is from slides6 Example 5.7
#' ## generate a sample of 9 from a normal distribution with sd=2
#' x <- rnorm(9, sd = 2)
#' ## the observed sample mean is 20
#' xx <- x- mean(x) + 20
#' ## find the posterior density
#' exmp1 <- normnorm(xx, m = 25, s = sqrt(10), sigma = 2)
#' ## summay and plot the example
#' summary(exmp1)
#' plot(exmp1, leg_pos = "right", cex = 0.8)
#'
#' ## this example assumes both mu and sigma are unknown
#' y <- rnorm(9)
#' exmp2 <- normnorm(y, m = 1, s = 2, a = 1, b = 1)
#' summary(exmp2)
#' ## show the first plot : Prior and Posterior Distribution of mu
#' plot(exmp2, which = 1, main = "Prior and Posterior Distribution of mu")
#' ## show the second plot : Prior and Posterior Distribution of tau
#' plot(exmp2, which = 2, col = 1:2)
#' ## show the third plot : Prior Contour
#' plot(exmp2, which = 3, main = "Prior Contour",
#'            xlim = c(-1,3),ylim = c(0,5) )
#' ## show the fourth plot : Posterior Contour
#' plot(exmp2, which = 4, main = "Posterior Contour",
#'            xlim = c(-3,3), ylim = c(0,5))
#' ## show the fifth plot : Prior and Posterior Contour
#' plot(exmp2, which = 5, main = "Prior and Posterior Contour",
#'            xlim = c(-3,3), ylim = c(0,5))
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
        tau <- seq(0,z, length.out = 500)
        tau.prior <- stats::dgamma(tau, shape = alpha, rate = beta)
        muu <- m + 5
        mul <- m - 5
        mu <- seq(mul, muu, length.out = 500)
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
     mu <- seq(z[1], z[2], length.out = 500)
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
  cat(paste("Posterior std. deviation : ",round(pos.std,4),"\n",sep=""))

  class(res) <- "g12post"
  invisible(res)
   }
 }


