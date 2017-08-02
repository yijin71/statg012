#'plot of normal
#'
#'plot of normal
#'
#'@examples
#'##plot example5.6
#'x <- rnorm(9, sd = 2)
#'xx <- x- mean(x) + 20
#'exmp1 <- normnorm(xx, m = 25, s = sqrt(10), sigma = 2)
#'plot.normal(exmp1)
#'
#'## if mu and sigma  are unknown
#'exmp2 <- normnorm(xx,m = 0.5, s = 1, a = 2, b = 2)
#'plot.normal(exmp2, which = "1")
#'plot.normal(exmp2, which = "2", xlim =range(16:32), ylim =range(0:10))
#'
#'@export plot.normal
plot.normal <- function (x, y, leg_pos = c("topright", "bottomright",
                                           "bottom", "bottomleft",
                                           "left", "topleft",
                                           "top", "right", "center"), which = "",...) {

  if(length(x) > 8) {
#############################################################################
# when sigma is unknown
  res <- x
#############################################################################
# mu, and mu.prior
  mu <- res$mu
  mu.prior <- res$mu.prior
  mu.pos <- res$mu.pos
#############################################################################
# tau, tau range and tau.prior
  tau <- res$tau
  tau.prior <-res$tau.prior
  tau.pos <- res$tau.pos

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
                         xlab = "mu",ylab = "density", ..., legend, fill,
                         border, angle, density, box.lwd, box.lty, box.col,
                         pt.bg, pt.cex, pt.lwd, xjust, yjust, x.intersp,
                         y.intersp, adj, text.width, text.col, text.font,
                         merge, trace, ncol, horiz, inset, title.col,
                         title.adj, seg.len, nlevels, levels, labels, labcex, drawlabels,
                         method, vfont, frame.plot, zlim) {
    graphics::matplot(xx, yy, col = col, xlab = xlab, ylab = ylab,
                      type = type, lty = lty, ...)
  }
  my_legend <- function(pos, lty = 1:3, lwd = 1, col = 1,
                        legend = c("mu.prior","mu.posterior"), ..., xlab, ylab,
                        lend, xlim, ylim, add, verbose, type, nlevels, levels,
                        labels, labcex, drawlabels,
                        method, vfont, frame.plot, zlim) {
    graphics::legend(pos, lty = lty, lwd = lwd, col = col, legend = legend,
                     ...)
  }
  my_matplot2 <- function(xx, yy, lty = 1:3, col = 1, type = "l",
                         xlab = "tau",ylab = "density", ..., legend, fill,
                         border, angle, density, box.lwd, box.lty, box.col,
                         pt.bg, pt.cex, pt.lwd, xjust, yjust, x.intersp,
                         y.intersp, adj, text.width, text.col, text.font,
                         merge, trace, ncol, horiz, inset, title.col,
                         title.adj, seg.len,nlevels, levels, labels, labcex, drawlabels,
                         method, vfont, frame.plot, zlim) {
    graphics::matplot(xx, yy, col = col, xlab = xlab, ylab = ylab,
                      type = type, lty = lty, ...)
  }
  my_legend2 <- function(pos, lty = 1:3, lwd = 1, col = 1,
                        legend = c("tau.prior","tau.posterior"), ..., xlab, ylab,
                        lend, xlim, ylim, add, verbose, type,nlevels, levels,
                        labels, labcex, drawlabels,
                        method, vfont, frame.plot, zlim) {
    graphics::legend(pos, lty = lty, lwd = lwd, col = col, legend = legend,
                     ...)
  }

  contour <- function(x,y,z, xlab = "mu", ylab="tau",
                      ..., legend, fill,
                      border, angle, density, box.lwd, box.lty, box.col,
                      pt.bg, pt.cex, pt.lwd, xjust, yjust, x.intersp,
                      y.intersp, adj, text.width, text.col, text.font,
                      merge, trace, ncol, horiz, inset, title.col,
                      title.adj, seg.len,lend, add, verbose, type){
    graphics::contour(x, y, z, xlab = xlab, ylab = ylab,
                      ...)
  }

  my_legend3 <- function(pos, lty = 1, lwd = 1, col = 1,
                         legend = c("prior contour"), ..., xlab, ylab,
                         lend, xlim, ylim, add, verbose, type,nlevels, levels,
                         labels, labcex, drawlabels,
                         method, vfont, frame.plot, zlim) {
    graphics::legend(pos, lty = lty, lwd = lwd, col = col, legend = legend,
                     ...)
  }

  if(which == "1") {
  par(mfrow=c(1,1))
  my_matplot(x, y, ...)
  my_legend(leg_pos, ...)
  }

  if (which == "2") {
    par(mfrow=c(1,1))
    my_matplot2(xx, yy, ...)
    my_legend2(leg_pos, ...)
  }

  if (which == "3") {
    par(mfrow=c(1,1))
  contour(mu, tau, outer(mu.prior,tau.prior),...)
  my_legend3(leg_pos, ...)

  }

  if (which == "4") {
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
                           xlab = "mu",ylab = "density", ..., legend, fill,
                           border, angle, density, box.lwd, box.lty, box.col,
                           pt.bg, pt.cex, pt.lwd, xjust, yjust, x.intersp,
                           y.intersp, adj, text.width, text.col, text.font,
                           merge, trace, ncol, horiz, inset, title.col,
                           title.adj, seg.len) {
      graphics::matplot(xx, yy, col = col, xlab = xlab, ylab = ylab,
                        type = type, lty = lty, ...)
    }
    my_legend <- function(pos, lty = 1:2, lwd = 1, col = 1,
                          legend = c("mu.prior", "posterior"), ..., xlab, ylab,
                          lend, xlim, ylim, add, verbose, type) {
      graphics::legend(pos, lty = lty, lwd = lwd, col = col, legend = legend,
                       ...)
    }
    par(mfrow= c(1,1))
    my_matplot(x, y, ...)
    my_legend(leg_pos, ...)

  }
}
