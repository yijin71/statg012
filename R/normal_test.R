#' Bayesian hypothesis testing with Normal distribution
#'
#'The Bayesian hypothesis testing is based on the posterior
#'distribution \eqn{\pi(\theta | x)}, and the decision is
#'to reject or accept the null hypothesis according to
#'which decision provides the smaller losses. The loss of
#'rejecting the null hypothesis is a times the probability
#'of the null is true, where a is the loss due to type I error.
#'The loss of accepting the null hypothesis is b times the probability
#'of the null is false, where b is the loss due to type II error.
#'
#'@param x results from the \code{\link{normnorm}} with known variance.
#'@param theta the parameter for hypothesis testing.
#'@param a loss due to type I error.
#'@param b loss due to typp II error.
#'@return hypothesis testing result and expected posterior loss
#'@source The
#'\href{https://moodle.ucl.ac.uk/mod/folder/view.php?id=2570901}{slides 9}
#'of STATG012 on Moodle
#'
#'@examples
#'## generate a sample of 16 from a normal distribution with sd=4
#'y <- rnorm(16, mean = 5.2, sd=4)
#'## the observed sample mean is 5.2
#'yy <- y- mean(y)+5.2
#'## find the posterior density
#'exmp1 <- normnorm(yy, m = 4.5, s = sqrt(10), sigma = 4)
#'## make the hypothesis testing
#'normaltest(exmp1, 5, 1, 1)
#'@export
normaltest <- function(x, theta, a, b) {
  res <-x
  pos.mean <- res$pos.mean
  pos.std <- res$pos.std
  p1 <- 1 - pnorm(abs(theta-pos.mean) / pos.std)
  p2 <- 1 -p1
  pos.loss1 <- p1 * a
  pos.loss2 <- p2 * b
  if (pos.loss1 <=  pos.loss2) {
    cat(paste("We reject H0 and the expected posterior loss is :", round(pos.loss1,5)))
  }
  if (pos.loss1 > pos.loss2) {
    cat(paste("We accept H0 and the expected posterior loss is :", round(pos.loss2,5)))
  }
}
