#'plot of normal
#'
#'plot of normal
#'
#'@examples
#'##plot example5.6
#'x <- rnorm(9, sd = 2)
#'xx <- x- mean(x) + 20
#'exmp1 <- normnorm(xx, m = 25, s = sqrt(10), sigma = 2)
#'plot.normal(exmp1, leg_pos = "right", cex = 0.8)
#'
#'## if mu and sigma  are unknown
#'y <- rnorm(13, mean = 2, sd = 0.5)
#'exmp2 <- normnorm(y, m = 0.5, s = 1, a = 2, b = 2)
#'## show the first plot : prior and posterior distribution of mu
#'plot.normal(exmp2, which = 1)
#'## show the second plot : prior and posterior distribution of tau
#'plot.normal(exmp2, which = 2, col = 1:2)
#'## show the third plot : Prior Contour
#'plot.normal(exmp2, which = 3, main = "Prior Contour",
#'            xlim = range(-3:3),ylim = range(0:3) )
#'## show the third plot : Posterior Contour
#'plot.normal(exmp2, which = 4, main = "Posterior Contour",
#'            xlim = range(-3:3), ylim = range(1:5))
#'@export plot.normal
plot.normal <- function (x, y, leg_pos = c("topright", "bottomright",
                                           "bottom", "bottomleft",
                                           "left", "topleft",
                                           "top", "right", "center"), which = c(1:4), ...) {

  if(length(x) > 9) {
#############################################################################
# when sigma is unknown
  res <- x
#############################################################################
# mu, and mu.prior
  mu <- res$mu
  mu.prior <- res$mu.prior
  mu.pos <- res$mu.posterior
#############################################################################
# tau, tau range and tau.prior
  tau <- res$tau
  tau.prior <-res$tau.prior
  tau.pos <- res$tau.posterior
  prior <- res$prior
  posterior <- res$posterior

  x <- cbind(mu, mu)
  y <- cbind(mu.prior, mu.pos)

  xx <- cbind(tau, tau)
  yy <- cbind(tau.prior, tau.pos)

  #
  leg_pos <- match.arg(leg_pos)
  user_args <- list(...)
  #
  my_matplot <- function(xx, yy, lty = 1:3, col = 1, type = "l",
                         xlab = "mu",ylab = "density",
                         main = "Prior and Posterior Distribution mu ",
                         ..., legend, fill,
                         border, angle, density, box.lwd, box.lty, box.col,
                         pt.bg, pt.cex, pt.lwd, xjust, yjust, x.intersp,
                         y.intersp, adj, text.width, text.col, text.font,
                         merge, trace, ncol, horiz, inset, title.col,
                         title.adj, seg.len, nlevels, levels, labels, labcex, drawlabels,
                         method, vfont, frame.plot, zlim) {
    graphics::matplot(xx, yy, col = col, xlab = xlab, ylab = ylab, main = main,
                      type = type, lty = lty, ...)
  }
  my_legend <- function(pos, lty = 1:3, lwd = 1, col = 1,
                        legend = c("mu.prior","mu.posterior"), ..., xlab, ylab,
                        lend, xlim, ylim, add, verbose, type, nlevels, levels,
                        labels, labcex, drawlabels,
                        method, vfont, frame.plot, zlim, main) {
    graphics::legend(pos, lty = lty, lwd = lwd, col = col, legend = legend,
                     ...)
  }
  my_matplot2 <- function(xx, yy, lty = 1:3, col = 1, type = "l",
                         xlab = "tau",ylab = "density",
                         main = "Prior and Posterior Distribution of mu ",
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
  my_legend2 <- function(pos, lty = 1:3, lwd = 1, col = 1,
                        legend = c("tau.prior","tau.posterior"), ..., xlab, ylab,
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

  my_legend3 <- function(pos, lty = 1, lwd = 1, col = 1,
                         legend = c("Contour"), ..., xlab, ylab,
                         lend, xlim, ylim, add, verbose, type,nlevels, levels,
                         labels, labcex, drawlabels,
                         method, vfont, frame.plot, zlim, main) {
    graphics::legend(pos, lty = lty, lwd = lwd, col = col, legend = legend,
                     ...)
  }

  if(which == 1) {
  par(mfrow=c(1,1))
  my_matplot(x, y, ...)
  my_legend(leg_pos, ...)
  }

  if (which == 2) {
    par(mfrow=c(1,1))
    my_matplot2(xx, yy, ...)
    my_legend2(leg_pos, ...)
  }

  if (which == 3) {
    par(mfrow=c(1,1))
  contour(mu, tau, outer(mu.prior,tau.prior),...)
  my_legend3(leg_pos, ...)

  }

  if (which == 4) {
    par(mfrow=c(1,1))
    contour(mu,tau,outer(mu.pos, tau.pos),...)
    my_legend3(leg_pos, ...)

  }

  } else {
#############################################################################
# when sigma is known
    res <- x
    mu.prior <- res$prior
    posterior <- res$posterior
    mu <- res$mu
    x <- cbind(mu, mu)
    x <- mu
    y <- cbind(mu.prior, posterior)
    #
    leg_pos <- match.arg(leg_pos)
    user_args <- list(...)
    #
    my_matplot <- function(xx, yy, lty = 1:2, col = 1, type = "l",
                           xlab = "mu",ylab = "density",
                           main =  "Prior and Posterior Distribution",
                           ..., legend, fill,
                           border, angle, density, box.lwd, box.lty, box.col,
                           pt.bg, pt.cex, pt.lwd, xjust, yjust, x.intersp,
                           y.intersp, adj, text.width, text.col, text.font,
                           merge, trace, ncol, horiz, inset, title.col,
                           title.adj, seg.len) {
      graphics::matplot(xx, yy, col = col, xlab = xlab, ylab = ylab, main = main,
                        type = type, lty = lty, ...)
    }
    my_legend <- function(pos, lty = 1:2, lwd = 1, col = 1,
                          legend = c("mu.prior", "posterior"), ..., xlab, ylab,
                          lend, xlim, ylim, add, verbose, type, main) {
      graphics::legend(pos, lty = lty, lwd = lwd, col = col, legend = legend,
                       ...)
    }
    par(mfrow= c(1,1))
    my_matplot(x, y, ...)
    my_legend(leg_pos, ...)

  }
}
