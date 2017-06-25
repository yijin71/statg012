#' Binomial sampling distribution with a beta distribution of prior
#'
#'Define the posterior distribution function for \eqn{\pi ( \theta | r )}, with
#'a beta prior distribution \eqn{\pi ( \theta; a, b )} and a binomial sampling
#'distribution \eqn{p ( r | \theta )}.
#'
#'@param alpha parameter for the beta distribution ( \eqn{\ge 0} ).
#'@param beta parameter for the beta distribution ( \eqn{\ge 0} ).
#'@param n the number of trials in binomial distribution.
#'@param r the number of successes in n trials.
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
#'binbeta(6, 2)
#'
#'## example 3.6 : 3 successes in 10 trials and a beta(4,6) prior
#'binbeta(4, 6, 10, 3)
#'
#'## 3 successes in 10 trials and a beta(4,6) prior, do not show the plot
#'binbeta(4, 6, 10, 3, plot = FALSE)
#'
#'@export

  binbeta <- function( alpha, beta, n, r, theta = seq(0,1,0.001)) {
  #############################################################################
  #alpha, beta: are parameters of beta distribution of prior ( >0)
  #theta: the range of the probability of success
  #n: no. of  binomial trials
  #r: no. of successes in n trials (0<= r<= n)

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
  res <- list(    prior = prior,
                  likelihood = likelihood,
                  posterior = posterior,
                  theta = theta,
                  pri.alpha = alpha,
                  pri.beta = beta,
                  pos.alpha = pos.alpha,
                  pos.beta = pos.beta)

  class(res) <- "binbeta"
  invisible(res)
  }
  #############################################################################
  #summary of the statistics

#' Summarizing binomial sampling distribution with a beta distribution of
#' prior
#'
#' A function used to produce summaries of prior and poesterior distribution,
#' based on binomial sampling distribution with a beta distribution of prior.
#'
#' @param objects an object of class "\code{g12binbeta}", usually, a result of a call
#' to \code{\link{binbeta}}.
#' @param ... further arguments from other methods
#' @return the function \code{summary.binbeta} returns a list of summary
#' statistics of prior and posterior distribution, repectively.
#' @export
   summary.binbeta <- function(objects, ...) {
    res <- objects
    ##statistics of prior function
    pri.alpha <- res$pri.alpha
    pri.beta <- res$pri.beta
    pri.mean <- pri.alpha / (pri.alpha+pri.beta)
    pri.var <- (pri.alpha * pri.beta) /
      ((pri.alpha + pri.beta) ^ 2 * (pri.alpha + pri.beta +1))
    pri.std <- sqrt(pri.var)
    pri.mode <- (pri.alpha - 1) / (pri.alpha + pri.beta - 2)
    prob <- c(0.0500, 0.2500, 0.5000, 0.7500, 0.9500)
    pri.qtl <- stats::qbeta(prob, pri.alpha, pri.beta)

    cat(paste("Prior Mean           : ",round(pri.mean, 5), "\n"))
    cat(paste("Prior Variance       : ",round(pri.var, 5), "\n"))
    cat(paste("Prior Std. Deviation : ",round(pri.std, 5), "\n"))
    cat(paste("Prior Mode           : ",round(pri.mode, 5), "\n"))
    cat("quantiles:", round(prob, 5), "\n")
    cat("\t", round(pri.qtl, 5), "\n")

    ##statistics of posterior function
    pos.alpha <- res$pos.alpha
    pos.beta <- res$pos.beta
    pos.mean <- pos.alpha/ (pos.alpha + pos.beta)
    pos.var <- (pos.alpha * pos.beta) /
      ((pos.alpha + pos.beta) ^ 2 * (pos.alpha + pos.beta +1))
    pos.std <- sqrt(pos.var)
    pos.mode <-
      if (pos.alpha > 1 & pos.beta > 1) {
        pos.mode <- (pos.alpha - 1) / (pos.alpha + pos.beta - 2)
      } else {
        "The mode only can be calculated when both parameters are larger than 1"
      }
    prob <- c(0.0500, 0.2500, 0.5000, 0.7500, 0.9500)
    pos.qtl <- stats::qbeta(prob, pos.alpha, pos.beta)

    cat(paste("Posterior Mean           : ",round(pos.mean, 5), "\n"))
    cat(paste("Posterior Variance       : ",round(pos.var, 5), "\n"))
    cat(paste("Posterior Std. Deviation : ",round(pos.std, 5), "\n"))
    cat(paste("Posterior Mode           : ",round(pos.mode, 5), "\n"))
    cat("quantiles:", round(prob, 5), "\n")
    cat("\t", round(pos.qtl, 5), "\n")
 ######################################################################

    results <- list (prior.variance = pri.var,
                     prior.std.deviation = pri.std,
                     prior.mode = pri.mode,
                     prior.quantiles = pri.qtl,
                     posteror.mean = pos.mean,
                     posteror.variance = pos.var,
                     posteror.std.deviation = pos.std,
                     posteror.mode = pos.mode,
                     posteror.quantiles = pos.qtl)
    class(results) <- "summary.g12binbeta"
    invisible(results)
  }


  #############################################################################
  # plots

  #if (plot) {
  # graphics::plot(posterior ~ theta, type = "l", lwd = 3,
  #                 xlab = expression(paste(theta,
  #                                        ": The Probability of Success")),
  #                ylab = "Probalbility Density Function",
  #                main = "The Prior, Likelihood and Posterior Distribution",
  #                col = "red")
  # lines(prior ~ theta, type = "l", lwd = 3, col = "blue")
  # lines(likelihood ~ theta,type = "l", lwd = 3, col = "green" )

  # legend("right", legend = c("Posterior Dist.",
   #                            "Prior Dist. ",
    #                           "Likelihood Dist."),
   #        bty = "o",
   #        cex = 1,
  #         lty = c(1, 1, 1),
  #         lwd = c(3, 3, 3),
   #        col = c("red","blue", "green"))
 # }

