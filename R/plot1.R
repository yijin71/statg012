plot1.binbeta <- function(objects, leg_pos = c("topright", "topleft"),
                         lty = c(1,2),...) {

  #
  res <- objects
  prior <- res$prior
  posterior <- res$posterior
  theta <- res$theta
  x <- cbind(theta, theta)
  y <- cbind(prior, posterior)

  leg_pos <- match.arg(leg_pos)

   #
  my_matplot <- function(xx, yy, ..., legend) {
    graphics::matplot(xx, yy, ...)
  }
  my_matplot(x, y, lty = lty, ...)
  #
  user_args <- list(...)
  if (is.null(user_args$legend)) {
    leg_text <- c("prior", "posterior")
    legend(leg_pos, legend = leg_text, lty = lty, ...)
  } else {
    legend(leg_pos, lty = lty, ...)
  }

}


########
#example:
a <- binbeta(4,6,10,3)
plot1.binbeta(a)
##  problem1 :lines of distribution are different from the legend

plot1.binbeta(a, "right") # will creat an error:Error in match.arg(leg_pos)





