
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
  class(results) <- "g12post"
  invisible(results)
}
