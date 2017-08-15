#'Generic Plotting of Prior and Posterior distribution.
#'
#'Plot the columns of one matrix against the columns of anothor. These matrices
#'come from different functions, including \code{\link{binombeta}},
#' \code{\link{nbinombeta}}, \code{\link{poisgamma}}, \code{\link{gamgam}} and
#' \code{\link{normnorm}}.
#'
#'@param x,y vectors or matrices of data for plotting. The number of row should
#'match. If one of them are missing, the other is taken as y and an x vector of
#'1:n is used. Missing values (NAs) are allowed.
#'@param leg_pos a character vector of legend place.
#'@param which if matrices are from \code{\link{normnorm}}, a subset of the plots
#'is required, specify a subset of the numbers 1:6. See below 'Details' for the
#'different kinds.
#'@param ... other arguments from \code{\link{matplot}}, \code{\link{legend}}
#'and \code{\link{contour}} to plotting functions.
#'@details
#'The 1 plot shows the prior and posterior distribution of mu.
#'The 2 plot shows the prior and posterior distribution of tau.
#'The 3 plot shows the prior contour.
#'The 4 plot shows the posterior contour.
#'@examples
#'## Get the posterior distribution with Gamma-Gamma model and plot.
#'y <- rgamma(10, shape = 3)
#'ex <- gamgam(y, a = 3, 4, 1)
#'plot(ex, leg_pos = "center" , box.lty=0)
#'## Obtain the posterior distribution with Normal-Gamma model,
#'## assuming both mu and sigma are unknown.
#'x <- rnorm(15, mean = 1, sd = 0.5)
#'ex <- normnorm(y, m = 2, s = 1, a = 1, b = 2)
#'## show the fourth plot
#'plot(ex, which = 4, main = "Posterior Contour",
#'            xlim = range(-3:3), ylim = range(0:3))
#'@export
plot.g12post <- function(x, y, leg_pos = c("topright", "bottomright",
                                           "bottom", "bottomleft",
                                           "left", "topleft",
                                           "top", "right", "center"),
                         which = c(1:4),...) {

  res <- x
  model <- x$model
  prior <- res$prior
  posterior <- res$posterior

  #
  leg_pos <- match.arg(leg_pos)
  user_args <- list(...)
  #

  my_matplot <- function(xx, yy, lty = 1:2, col = 1, type = "l",
                         xlab = "theta",ylab = "density",
                         main="Prior and Posterior Distribution",..., legend, fill,
                         border, angle, density, box.lwd, box.lty, box.col,
                         pt.bg, pt.cex, pt.lwd, xjust, yjust, x.intersp,
                         y.intersp, adj, text.width, text.col, text.font,
                         merge, trace, ncol, horiz, inset, title.col,
                         title.adj, seg.len) {
    graphics::matplot(xx, yy, col = col, xlab = xlab, ylab = ylab, main=main,
                      type = type, lty = lty, ...)
  }

  my_matplot2 <- function(xx, yy, lty = 1:2, col = 1, type = "l", xlab = "mu",
                         ylab = "density",main="Prior and Posterior Distribution",
                         ..., legend, fill, border, angle,
                         density, box.lwd, box.lty, box.col, pt.bg, pt.cex,
                         pt.lwd, xjust, yjust, x.intersp, y.intersp, adj,
                         text.width, text.col, text.font, merge, trace, ncol,
                         horiz, inset, title.col, title.adj, seg.len) {
    graphics::matplot(xx, yy, col = col, xlab = xlab, ylab = ylab,
                      main = main, type = type,
                      lty = lty, ...)
  }

  my_matplot3 <- function(xx, yy, lty = 1:2, col = 1, type = "l",
                          xlab = "tau",ylab = "density",
                          main = "Prior and Posterior Distribution of tau ",
                          ..., legend, fill,
                          border, angle, density, box.lwd, box.lty, box.col,
                          pt.bg, pt.cex, pt.lwd, xjust, yjust, x.intersp,
                          y.intersp, adj, text.width, text.col, text.font,
                          merge, trace, ncol, horiz, inset, title.col,
                          title.adj, seg.len,nlevels, levels, labels, labcex, drawlabels,
                          method, vfont, frame.plot, zlim) {
    graphics::matplot(xx, yy, col = col, xlab = xlab, ylab = ylab, main = main,
                      type = type, lty = lty, ...)
  }


  my_legend <- function(pos, lty = 1:2, lwd = 1, col = 1,
                        legend = c("prior", "posterior"), ..., xlab, ylab,
                        lend, xlim, ylim, add, verbose, type, main) {
    graphics::legend(pos, lty = lty, lwd = lwd, col = col, legend = legend, ...)
  }

  my_legend2 <- function(pos, lty = 1:2, lwd = 1, col = 1,
                        legend = c("mu.prior","mu.posterior"), ..., xlab, ylab,
                        lend, xlim, ylim, add, verbose, type, nlevels, levels,
                        labels, labcex, drawlabels,
                        method, vfont, frame.plot, zlim, main) {
    graphics::legend(pos, lty = lty, lwd = lwd, col = col, legend = legend,
                     ...)
  }

  my_legend3 <- function(pos, lty = 1:2, lwd = 1, col = 1,
                         legend = c("tau.prior","tau.posterior"), ..., xlab, ylab,
                         lend, xlim, ylim, add, verbose, type,nlevels, levels,
                         labels, labcex, drawlabels,
                         method, vfont, frame.plot, zlim, main) {
    graphics::legend(pos, lty = lty, lwd = lwd, col = col, legend = legend,
                     ...)
  }

  my_legend4 <- function(pos, lty = 1, lwd = 1, col = 1,
                         legend = c("Contour"), ..., xlab, ylab,
                         lend, xlim, ylim, add, verbose, type,nlevels, levels,
                         labels, labcex, drawlabels,
                         method, vfont, frame.plot, zlim, main) {
    graphics::legend(pos, lty = lty, lwd = lwd, col = col, legend = legend,
                     ...)
  }

  contour <- function(x,y,z, xlab = "mu", ylab="tau",
                      main = "",..., legend, fill,
                      border, angle, density, box.lwd, box.lty, box.col,
                      pt.bg, pt.cex, pt.lwd, xjust, yjust, x.intersp,
                      y.intersp, adj, text.width, text.col, text.font,
                      merge, trace, ncol, horiz, inset, title.col,
                      title.adj, seg.len,lend, add, verbose, type){
    graphics::contour(x, y, z, xlab = xlab, ylab = ylab, main = main,
                      ...)
  }
  if ( model == "binombeta") {

    theta <- res$theta
    x <- cbind(theta, theta)
    y <- cbind(prior, posterior)
    my_matplot(x, y, ...)
    my_legend(leg_pos, ...)
  }

  if ( model == "nbinombeta") {
    theta <- res$theta
    x <- cbind(theta, theta)
    y <- cbind(prior, posterior)
    my_matplot(x, y, ...)
    my_legend(leg_pos, ...)
  }

  if ( model == "poisgamma") {

    mu <- res$mu
    x <- cbind(mu, mu)
    y <- cbind(prior, posterior)
     my_matplot2(x, y, ...)
    my_legend(leg_pos, ...)
  }

  if (model == "gamgam") {

    theta <- res$theta
    x <- cbind(theta, theta)
    y <- cbind(prior, posterior)
    my_matplot(x, y, ...)
    my_legend(leg_pos, ...)
  }

  if (model == "normnorm") {

    if(length(x) > 11) {
    #############################################################################
    # when sigma is unknown
    mu <- res$mu
    mu.prior <- res$mu.prior
    mu.pos <- res$mu.posterior
    tau <- res$tau
    tau.prior <-res$tau.prior
    tau.pos <- res$tau.posterior

    x <- cbind(mu, mu)
    y <- cbind(mu.prior, mu.pos)

    xx <- cbind(tau, tau)
    yy <- cbind(tau.prior, tau.pos)

    if(which == 1) {
      my_matplot2(x, y, ...)
      my_legend2(leg_pos, ...)
    }

    if (which == 2) {
      my_matplot3(xx, yy, ...)
      my_legend3(leg_pos, ...)
    }

    if (which == 3) {
      contour(mu, tau, outer(mu.prior,tau.prior),...)
      my_legend4(leg_pos, ...)

    }

    if (which == 4) {
      contour(mu,tau,outer(mu.pos, tau.pos),...)
      my_legend4(leg_pos, ...)

    }

  } else {
    #############################################################################
    # when sigma is known
    mu.prior <- res$prior
    mu <- res$mu
    x <- cbind(mu, mu)
    y <- cbind(mu.prior, posterior)
    my_matplot2(x, y, ...)
    my_legend2(leg_pos, ...)
  }
  }

}









