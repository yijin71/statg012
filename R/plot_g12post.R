#'Plot shows the prior and posterior distribution
#'
#'@param x,y the vectors or matrices of data for ploting
#'@param leg_pos the place to put the legend
#'@param model "binbeta", "poigamma" ...
#'@export
plot.g12post <- function(x, y, leg_pos = c("topright", "bottomright",
                                           "bottom", "bottomleft",
                                           "left", "topleft",
                                           "top", "right", "center"), ...) {

  if (x$model == "binombeta") {
    #
    res <- x
    prior <- res$prior
    posterior <- res$posterior
    theta <- res$theta
    x <- cbind(theta, theta)
    x <- theta
    y <- cbind(prior, posterior)

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
    my_legend <- function(pos, lty = 1:2, lwd = 1, col = 1,
                          legend = c("prior", "posterior"), ..., xlab, ylab,
                          lend, xlim, ylim, add, verbose, type, main) {
      graphics::legend(pos, lty = lty, lwd = lwd, col = col, legend = legend, ...)
    }
    my_matplot(x, y, ...)
    my_legend(leg_pos, ...)

  }

  if (x$model == "poisgamma") {
    #
    res <- x
    prior <- res$prior
    posterior <- res$posterior
    mu <- res$mu
    x <- cbind(mu, mu)
    x <- mu
    y <- cbind(prior, posterior)

    #
    leg_pos <- match.arg(leg_pos)
    user_args <- list(...)
    #
    my_matplot <- function(xx, yy, lty = 1:2, col = 1, type = "l", xlab = "mu",
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
    my_legend <- function(pos, lty = 1:2, lwd = 1, col = 1,
                          legend = c("prior", "posterior"), ..., xlab, ylab,
                          lend, xlim, ylim, add, verbose, type, main) {
      graphics::legend(pos, lty = lty, lwd = lwd, col = col, legend = legend,
                       ...)
    }
    my_matplot(x, y, ...)
    my_legend(leg_pos, ...)
  }

  if (x$model == "gamgam") {
    #
    res <- x
    prior <- res$prior
    posterior <- res$posterior
    theta <- res$theta
    x <- cbind(theta, theta)
    x <- theta
    y <- cbind(prior, posterior)
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
    my_legend <- function(pos, lty = 1:2, lwd = 1, col = 1,
                          legend = c("prior", "posterior"), ..., xlab, ylab,
                          lend, xlim, ylim, add, verbose, type, main) {
      graphics::legend(pos, lty = lty, lwd = lwd, col = col, legend = legend, ...)
    }
    my_matplot(x, y, ...)
    my_legend(leg_pos, ...)

  }
}









