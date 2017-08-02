#' show problem of normal when mu and sigma are unknown
#'
#'@param m and s are parameters of mu which follows normal distribution
#'@param alpha and beta are parameters of tau(1/sigma^2) which follows gamma
#'distribution
#'
#'@examples
#'##
#'x <- rnorm(10, sd = 1)
#'a <- normalprob(x, m = 2, s = 1, a = 2, b = 2)
#'
#'## problem1: the posterior is almost zero
#'a$posterior
#'
#'## problem2 : the mu.prior distribution is more concentrated than the mu.posterior,
#'##for tau is fine, but for all prior and posterior is not, because posterior is 0.
#'##black line is prior and red line is posterior
#'muplot <- matplot(cbind(a$mu,a$mu),cbind(a$mu.prior,a$mu.pos),col = 1:2)
#'tauplot <- matplot(,cbind(a$tau.prior,a$tau.pos),col = 1:2)
#'all <- matplot(cbind(a$prior,a$posterior),col = 1:2)
#'
#'@export normalprob

normalprob <- function (x, m, s, alpha, beta){
######################################################################
# mu and tau prior
z <- stats::qnorm(0.9999, m, s)
mu <- seq(0, z, z/1000)
mu.prior <- stats::dnorm(mu, m, s)

zz<- stats::qgamma(0.9999, alpha, beta)
tau <- seq(0, zz, zz/length(mu))
tau <- tau[1:length(tau) - 1]
tau.prior <- stats::dgamma(tau, alpha, beta)

#formula from wikipedia article"Normal-gamma distribution"
#"Posterior distribution of the parameters" part
prior <- tau ^ (alpha - 0.5) * exp(- beta * tau) *
  exp(-0.5 * s * tau * (mu - m) ^ 2)

######################################################################
#likelihood x~N(mu, tao^-1)
n <- length(x)
x_mean <- mean(x)
s_var <- 1 / n * sum((x - x_mean) ^ 2)
likelihood <- tau ^ (n / 2) * exp((-tau/2) *
                                (n * s_var + n * (x_mean-mu) ^2))

######################################################################
#posterior
pos.m <- (s * m + n * x_mean) / (s + n)
pos.s <- s + n
pos.alpha <- alpha + n / 2
pos.beta <- beta + 0.5 * (n * s_var +
                            (s * n * (x_mean - m)^2 /pos.s))

mu.pos <- stats::dnorm(mu, pos.m, pos.s)
tau.pos <- stats::dgamma(tau, pos.alpha, pos.beta)

posterior <- tau.pos ^(pos.alpha - 0.5) * exp(-tau.pos * pos.beta) *
exp(-tau.pos/2 * pos.s * (mu.pos - pos.beta)^2)

res <- list( mu = mu,
             tau = tau,
             mu.prior = mu.prior,
             tau.prior = tau.prior,
             prior = prior,
             likelihood = likelihood,
             mu.pos = mu.pos,
             tau.pos = tau.pos,
             posterior = posterior,
             pos.m =  pos.m,
             pos.s =  pos.s,
             pos.alpha = pos.alpha,
             pos.beta = pos.beta)

cat(paste("Posterior mu0      : ", round(pos.m,4),"\n",sep=""))
cat(paste("Posterior lamda0   : ", round(pos.s,4),"\n",sep=""))
cat(paste("Posterior alpha    : ", round(pos.alpha,4),"\n",sep=""))
cat(paste("Posterior beta     : ", round(pos.beta,4),"\n",sep=""))

invisible(res)

}

