% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gamgam.R
\name{gamgam}
\alias{gamgam}
\title{Gamma sampling distribution with a gamma distribution of prior}
\source{
For theory details, based on
\href{https://moodle.ucl.ac.uk/mod/folder/view.php?id=2570901}{STATG012
slides 5} Example 5.4 from Moodle at UCL.
}
\usage{
gamgam(t, a = 1, shape, rate, scale = 1/rate)
}
\arguments{
\item{t}{a sample data from a gamma distribution with shape a and
rate \eqn{\theta} ( t > 0 ).}

\item{a}{the known shape parameter of the gamma sampling distribution
(a > 0).}

\item{shape}{the shape parameter of the prior gamma distribution for
unknown \eqn{\theta} (\eqn{\alpha} > 0).}

\item{rate}{the rate parameter of the prior gamma distribution for
unknown \eqn{\theta} (\eqn{\beta} > 0).}

\item{scale}{equals 1 / rate ( > 0).}
}
\value{
An object of class "\code{g12post}" is returned.
\item{prior}{the prior distribution for unknown \eqn{\theta} , i.e.
the \eqn{gamma(\alpha,\beta}) distribution.} \item{likelihood}{the likelihood
function of t given \eqn{\theta}, i.e. the
\eqn{f(t | \theta)} distribution.} \item{posterior}{the
posterior distribution of \eqn{\theta} given t.} \item{theta}{the
unknown rate parameter of the gamma sampling distribution t
\eqn{\pi ( \theta; \alpha, \beta )}.} \item{pri.shape}{the
shape parameter of the gamma distribution for prior.} \item{pri.rate}{the
rate parameter of the gamma distribution for prior.} \item{pos.shape}{the
shape parameter of the gamma distribution for
posterior.} \item{pos.rate}{the rate parameter of the gamma distribution
for posterior.}\item{model}{the prior and likelihood type to produce the
posterior.}
}
\description{
Define the posterior distribution function for \eqn{\pi (\theta | t )},
with a gamma prior distribution \eqn{\pi ( \theta; \alpha, \beta )} and a
gamma sampling distribution with known shape parameter a and unknown rate
parameter\eqn{\theta}.
}
\examples{
## an exponential distribution with a gamma prior, similiar
## as Example 5.4 from slides 6
## generate a sample of 10 observations from an exponential distribution
x <- rexp(10)
## find the posterior density and summary it
gam <- gamgam(x, a = 1, 4, 2)
summary(gam)

## generate a sample of 50 observations from a gamma distribution with a = 2
y <- rgamma(10, shape = 2)
## find the posterior density and plot it
ex <- gamgam(y, a = 2, 4, 2)
plot(ex, leg_pos = "center" , box.lty=0)
}
\references{
Fink, D. 1997. A Compendium of Conjugate Priors.
\href{https://www.researchgate.net/publication/238622435_A_Compendium_of_Conjugate_Priors}{Weblink}.
}
\seealso{
\code{\link{summary.g12post}} for summararies of prior
and posterior distribution.

\code{\link{plot.g12post}} for plots of prior and posterior
distribution.
}
