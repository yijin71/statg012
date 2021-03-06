% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binombeta.R
\name{binombeta}
\alias{binombeta}
\title{Binomial sampling distribution with a beta distribution of prior}
\source{
For code \code{binombeta}, based on
\preformatted{Curren,J.(2017) R topics documentated:bingcp-Package
'Bolstad'. Pp15.}

For the theory, based on
\preformatted{The STATG012 slides2 Example3.6 on Moodle at UCL.}
\href{https://moodle.ucl.ac.uk/mod/folder/view.php?id=2570901}{STATG012
slides2}
}
\usage{
binombeta(alpha = 1, beta = 1, n, r, theta = seq(0, 1, 0.001))
}
\arguments{
\item{alpha}{the parameter for the beta distribution ( \eqn{\ge 0} ).}

\item{beta}{the parameter for the beta distribution ( \eqn{\ge 0} ).}

\item{n}{the number of trials in binomial distribution.}

\item{r}{the number of successes in n trials.}

\item{theta}{the range of the probability of success.}
}
\value{
An object of class "\code{g12post}" is returned.
\item{prior}{the prior distribution, i.e.
the \eqn{beta(\alpha,\beta)} distribution.} \item{likelihood}{the likelihood
function of r given \eqn{\theta}, i.e. the
\eqn{binomial(n,r)} distribution.} \item{posterior}{the
posterior distribution of \eqn{\theta} given r, i.e. the
\eqn{beta(\alpha+r, \beta+n-r)} distribution.} \item{theta}{the
range of the probability of success.} \item{pri.alpha}{the
alpha parameter for the beta distribution of prior.} \item{pri.beta}{the
beta parameter for the beta distribution of prior.} \item{pos.alpha}{the
alpha parameter for the beta distribution of posterior.} \item{pos.beta}{the
beta parameter for the beta distribution of posterior.}\item{model}{the
prior and likelihood type to produce the posterior.}
}
\description{
Define the posterior distribution function for \eqn{\pi ( \theta | r )}, with
a beta prior distribution \eqn{\pi ( \theta; \alpha, \beta )} and a binomial
sampling distribution \eqn{p ( r | \theta )}.
}
\examples{

## simplest one with 3 successes in 10 trials and a constant
## (uniform beta(1,1)) prior, then the posterior distribution
## has exactly the same shape as the likelihood function.
a <- binombeta(n = 6, r = 2)
summary(a)

## example 3.6 : 3 successes in 10 trials and a beta(4,6) prior.
ex <- binombeta(4, 6, 10, 3)
plot(ex, lty = 1:2, col = c(4,2))

}
\references{
Bolstad, W.M. 2007. Introduction to Bayesian Statistics. (2nd ed.).
Hoboken, New Jersey: John Wiley & Sons, Inc.
}
\seealso{
\code{\link{summary.g12post}} for summararies of prior
and posterior distribution.

\code{\link{plot.g12post}} for plots of prior and posterior
distribution.
}
