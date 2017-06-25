plot.binbeta <- function(objects, ...) {

  #
  res <- objects
  prior <- res$prior
  posterior <- res$posterior
  theta <- res$theta
  x <- cbind(theta, theta)
  y <- cbind(prior, posterior)

  #
  user_args <- list(...)
  leg_text <- c("prior", "posterior")

  ### die here
  if (is.null(user_args$legend)) {
    graphics::matplot(x, y, ...)
    graphics::legend("topright", legend = leg_text)
  } else {
    graphics::matplot(x, y)
    graphics::legend(legend = leg_text, ...)
    }
}
