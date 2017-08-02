
#' Summarizing binomial sampling distribution with a beta distribution of
#' prior
#'
#' A function used to produce summaries of prior and poesterior distribution,
#' based on binomial sampling distribution with a beta distribution of prior.
#'
#' @param objects an object of class "\code{g12post}", usually, a result of a call
#' to \code{\link{binbeta}}.
#' @param ... further arguments from other methods
#' @return the function \code{summary.g12post} returns a list of summary
#' statistics of prior and posterior distribution, repectively.
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

  results <- list (prior.variance = pri.var,
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

    results <- list (prior.variance = pri.var,
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

    results <- list (prior.variance = pri.var,
                     prior.std.deviation = pri.std,
                     prior.quantiles = pri.qtl,
                     posteror.mean = pos.mean,
                     posteror.variance = pos.var,
                     posteror.std.deviation = pos.std,
                     posteror.quantiles = pos.qtl)
    class(results) <- "g12post"
    invisible(results)
  }
}
