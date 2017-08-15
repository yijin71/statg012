
#' Object Summaries
#'
#' A generic function used to produce result summaries of the results of various
#' model fitthing functions. The distinctive models are \code{\link{binombeta}},
#' \code{\link{nbinombeta}}, \code{\link{poisgamma}}, \code{\link{gamgam}} and
#' \code{\link{normnorm}} respectively.
#'
#' @param objects an object for which a summary is desired.
#' @param ... additional arguments affecting the summary produced.
#' @return the function \code{summary.g12post} returns a list of summary
#' statistics such as mean, variance and quantiles of prior and posterior
#' distribution, repectively.
#' @examples
#' ## Obtain the posterior distribution with binomial-beta model and summary
#' ## it
#' ep1 <- binombeta(alpha = 1, beta = 2, n = 10, r = 3)
#' summary(ep1)
#' @export
summary.g12post <- function(objects, ...) {

  if(objects$model == "binombeta") {
  res <- objects
  ##statistics of prior function
  pri.alpha <- res$pri.alpha
  pri.beta <- res$pri.beta
  pri.mean <- pri.alpha / (pri.alpha+pri.beta)
  pri.var <- (pri.alpha * pri.beta) /
    ((pri.alpha + pri.beta) ^ 2 * (pri.alpha + pri.beta +1))
  pri.std <- sqrt(pri.var)
  prob <- c(0.0500, 0.2500, 0.5000, 0.7500, 0.9500)
  pri.qtl <- stats::qbeta(prob, pri.alpha, pri.beta)

  cat(paste("Prior Mean           : ",round(pri.mean, 4), "\n"))
  cat(paste("Prior Variance       : ",round(pri.var, 4), "\n"))
  cat(paste("Prior Std. Deviation : ",round(pri.std, 4), "\n"))
  cat("prob.", "quantiles","\n")
  for (i in 1:5) {
    cat(prob[i],round(pri.qtl[i],3),sep="\t")
    cat("\n")
  }
  cat("\n")
  ##statistics of posterior function
  pos.alpha <- res$pos.alpha
  pos.beta <- res$pos.beta
  pos.mean <- pos.alpha/ (pos.alpha + pos.beta)
  pos.var <- (pos.alpha * pos.beta) /
    ((pos.alpha + pos.beta) ^ 2 * (pos.alpha + pos.beta +1))
  pos.std <- sqrt(pos.var)
  prob <- c(0.0500, 0.2500, 0.5000, 0.7500, 0.9500)
  pos.qtl <- stats::qbeta(prob, pos.alpha, pos.beta)

  cat(paste("Posterior Mean           : ",round(pos.mean, 4), "\n"))
  cat(paste("Posterior Variance       : ",round(pos.var, 4), "\n"))
  cat(paste("Posterior Std. Deviation : ",round(pos.std, 4), "\n"))
  cat("prob.", "quantiles","\n")
  for (i in 1:5) {
    cat(prob[i],round(pos.qtl[i],4),sep="\t")
    cat("\n")
  }
  ######################################################################

  results <- list (pri.mean = pri.mean,
                   prior.variance = pri.var,
                   prior.std.deviation = pri.std,
                   prior.quantiles = pri.qtl,
                   posteror.mean = pos.mean,
                   posteror.variance = pos.var,
                   posteror.std.deviation = pos.std,
                   posteror.quantiles = pos.qtl)
  class(results) <- "g12post"
  invisible(results)
  }

  if(objects$model == "nbinombeta") {
    res <- objects
    ##statistics of prior function
    pri.alpha <- res$pri.alpha
    pri.beta <- res$pri.beta
    pri.mean <- pri.alpha / (pri.alpha+pri.beta)
    pri.var <- (pri.alpha * pri.beta) /
      ((pri.alpha + pri.beta) ^ 2 * (pri.alpha + pri.beta +1))
    pri.std <- sqrt(pri.var)
    prob <- c(0.0500, 0.2500, 0.5000, 0.7500, 0.9500)
    pri.qtl <- stats::qbeta(prob, pri.alpha, pri.beta)

    cat(paste("Prior Mean           : ",round(pri.mean, 4), "\n"))
    cat(paste("Prior Variance       : ",round(pri.var, 4), "\n"))
    cat(paste("Prior Std. Deviation : ",round(pri.std, 4), "\n"))
    cat("prob.", "quantiles","\n")
    for (i in 1:5) {
      cat(prob[i],round(pri.qtl[i],3),sep="\t")
      cat("\n")
    }
    cat("\n")
    ##statistics of posterior function
    pos.alpha <- res$pos.alpha
    pos.beta <- res$pos.beta
    pos.mean <- pos.alpha/ (pos.alpha + pos.beta)
    pos.var <- (pos.alpha * pos.beta) /
      ((pos.alpha + pos.beta) ^ 2 * (pos.alpha + pos.beta +1))
    pos.std <- sqrt(pos.var)
    prob <- c(0.0500, 0.2500, 0.5000, 0.7500, 0.9500)
    pos.qtl <- stats::qbeta(prob, pos.alpha, pos.beta)

    cat(paste("Posterior Mean           : ",round(pos.mean, 4), "\n"))
    cat(paste("Posterior Variance       : ",round(pos.var, 4), "\n"))
    cat(paste("Posterior Std. Deviation : ",round(pos.std, 4), "\n"))
    cat("prob.", "quantiles","\n")
    for (i in 1:5) {
      cat(prob[i],round(pos.qtl[i],4),sep="\t")
      cat("\n")
    }
    ######################################################################

    results <- list (pri.mean = pri.mean,
                     prior.variance = pri.var,
                     prior.std.deviation = pri.std,
                     prior.quantiles = pri.qtl,
                     posteror.mean = pos.mean,
                     posteror.variance = pos.var,
                     posteror.std.deviation = pos.std,
                     posteror.quantiles = pos.qtl)
    class(results) <- "g12post"
    invisible(results)
  }

  if (objects$model == "poisgamma") {
    res <- objects
    pri.s <- res$pri.shape
    pri.r <- res$pri.rate
    pri.mean <- pri.s / pri.r
    pri.var <- pri.s / pri.r^2
    pri.std <- sqrt(pri.var)
    prob <- c(0.0500, 0.2500, 0.5000, 0.7500, 0.9500)
    pri.qtl <- stats::qgamma(prob, pri.s, pri.r)

    cat(paste("Prior Mean           : ",round(pri.mean, 4), "\n"))
    cat(paste("Prior Variance       : ",round(pri.var, 4), "\n"))
    cat(paste("Prior Std. Deviation : ",round(pri.std, 4), "\n"))
    cat("prob.", "quantiles","\n")
    for (i in 1:5) {
      cat(prob[i],round(pri.qtl[i],4),sep="\t")
      cat("\n")
    }
    cat("\n")

    pos.s <- res$pos.shape
    pos.r <- res$pos.rate
    pos.mean <- pos.s / pos.r
    pos.var <- pos.s / pos.r^2
    pos.std <- sqrt(pos.var)
    prob <- c(0.0500, 0.2500, 0.5000, 0.7500, 0.9500)
    pos.qtl <- stats::qgamma(prob, pos.s, pos.r)
    cat(paste("Posterior Mean           : ",round(pos.mean, 4), "\n"))
    cat(paste("Posterior Variance       : ",round(pos.var, 4), "\n"))
    cat(paste("Posterior Std. Deviation : ",round(pos.std, 4), "\n"))
    cat("prob.", "quantiles","\n")
    for (i in 1:5) {
      cat(prob[i],round(pos.qtl[i],4),sep="\t")
      cat("\n")
    }

    results <- list (pri.mean = pri.mean,
                     prior.variance = pri.var,
                     prior.std.deviation = pri.std,
                     prior.quantiles = pri.qtl,
                     posteror.mean = pos.mean,
                     posteror.variance = pos.var,
                     posteror.std.deviation = pos.std,
                     posteror.quantiles = pos.qtl)
    class(results) <- "g12post"
    invisible(results)

  }



  if (objects$model == "gamgam"){
    res <- objects
    pri.s <- res$pri.shape
    pri.r <- res$pri.rate
    pri.mean <- pri.s / pri.r
    pri.var <- pri.s / pri.r^2
    pri.std <- sqrt(pri.var)
    prob <- c(0.0500, 0.2500, 0.5000, 0.7500, 0.9500)
    pri.qtl <- stats::qgamma(prob, pri.s, pri.r)

    cat(paste("Prior Mean           : ",round(pri.mean, 4), "\n"))
    cat(paste("Prior Variance       : ",round(pri.var, 4), "\n"))
    cat(paste("Prior Std. Deviation : ",round(pri.std, 4), "\n"))
    cat("prob.", "quantiles","\n")
    for (i in 1:5) {
      cat(prob[i],round(pri.qtl[i],4),sep="\t")
      cat("\n")
    }
    cat("\n")

    pos.s <- res$pos.shape
    pos.r <- res$pos.rate
    pos.mean <- pos.s / pos.r
    pos.var <- pos.s / pos.r^2
    pos.std <- sqrt(pos.var)
    prob <- c(0.0500, 0.2500, 0.5000, 0.7500, 0.9500)
    pos.qtl <- stats::qgamma(prob, pos.s, pos.r)
    cat(paste("Posterior Mean           : ",round(pos.mean, 4), "\n"))
    cat(paste("Posterior Variance       : ",round(pos.var, 4), "\n"))
    cat(paste("Posterior Std. Deviation : ",round(pos.std, 4), "\n"))
    cat("prob.", "quantiles","\n")
    for (i in 1:5) {
      cat(prob[i],round(pos.qtl[i],4),sep="\t")
      cat("\n")
    }

    results <- list (pri.mean = pri.mean,
                     prior.variance = pri.var,
                     prior.std.deviation = pri.std,
                     prior.quantiles = pri.qtl,
                     posteror.mean = pos.mean,
                     posteror.variance = pos.var,
                     posteror.std.deviation = pos.std,
                     posteror.quantiles = pos.qtl)
    class(results) <- "g12post"
    invisible(results)
  }

  if (objects$model == "normnorm"){
    res <- objects
    if (length(res) > 11) {
      pos.m <- res$pos.m
      pos.s <- res$pos.s
      pos.alpha <- res$pos.alpha
      pos.beta <- res$pos.beta

      x.mean <- pos.m
      x.var <- pos.beta /(pos.s * (pos.alpha - 1))
      x.std <- sqrt(x.var)
      tau.mean <- pos.alpha / pos.beta
      tau.var <- pos.alpha / (pos.beta^2)
      tau.std <- sqrt(tau.var)

      cat(paste("X Mean              : ",round(x.mean, 4), "\n"))
      cat(paste("X Variance          : ",round(x.var, 4), "\n"))
      cat(paste("X Std. Deviation    : ",round(x.std, 4), "\n"))
      cat("\n")
      cat(paste("Tau Mean      : ",round(tau.mean, 4), "\n"))
      cat(paste("Tau Variance  : ",round(tau.var, 4), "\n"))
      cat(paste("Tau Deviation : ",round(tau.std, 4), "\n"))

       results <- list (x.mean = x.mean,
                        x.variance = x.var,
                        x.std.deviation = x.std,
                        tau.mean = tau.mean,
                        tau.variance = tau.var,
                        tau.std.deviation = tau.std
                         )

      class(results) <- "g12post"
      invisible(results)
    } else {

      pri.precision <- res$pri.precision
      pri.mean <- res$pri.mean
      pri.std  <- res$pri.std
      pri.var <- pri.std^2
      prob <- c(0.0500, 0.2500, 0.5000, 0.7500, 0.9500)
      pri.qtl <- stats::qnorm(prob, pri.mean, pri.std)
      cat(paste("Prior Precision      : ",round(pri.precision, 4), "\n"))
      cat(paste("Prior Mean           : ",round(pri.mean, 4), "\n"))
      cat(paste("Prior Variance       : ",round(pri.var, 4), "\n"))
      cat(paste("Prior Std. Deviation : ",round(pri.std, 4), "\n"))
      cat("prob.", "quantiles","\n")
      for (i in 1:5) {
        cat(prob[i],round(pri.qtl[i],4),sep="\t")
        cat("\n")
      }

      pos.precision <-res$pos.precision
      pos.mean <- res$pos.mean
      pos.std <- res$ pos.std
      pos.var <- pos.std^2

      pos.qtl <- stats::qnorm(prob, pos.mean, pos.std)
      cat(paste("Posterior Precision      : ",round(pos.precision, 4), "\n"))
      cat(paste("Posterior Mean           : ",round(pos.mean, 4), "\n"))
      cat(paste("Posterior Variance       : ",round(pos.var, 4), "\n"))
      cat(paste("Posterior Std. Deviation : ",round(pos.std, 4), "\n"))
      cat("prob.", "quantiles","\n")
      for (i in 1:5) {
        cat(prob[i],round(pos.qtl[i],4),sep="\t")
        cat("\n")
      }

      results <- list (pri.precision = pri.precision,
                       pri.mean = pri.mean,
                       prior.variance = pri.var,
                       prior.std.deviation = pri.std,
                       prior.quantiles = pri.qtl,
                       pos.precision = pos.precision,
                       posteror.mean = pos.mean,
                       posteror.variance = pos.var,
                       posteror.std.deviation = pos.std,
                       posteror.quantiles = pos.qtl)
      class(results) <- "g12post"
      invisible(results)

    }

  }
}
