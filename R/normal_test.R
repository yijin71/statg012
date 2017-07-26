#' Bayesian hypothesis testing with Normal distribution
#'
#'
#'@param x results from normal
#'@param theta the parameter for hypothesis testing.
#'@param a type I error
#'@param b typp II error
#'@export
normaltest <- function(x, theta, a, b) {
  res <-x
  pos.mean <- res$pos.mean
  pos.var <- res$pos.var
  p1 <- abs(pos.mean - theta) / sqrt(pos.var)
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
