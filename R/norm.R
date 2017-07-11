#'Normal distribution with a normal prior
#'
#'@export
norm <- function(x, m.mu, s.mu, s.x, mu = NULL){

#prior g(mu) ~ N(m.mu,s.mu^2)
#likelihood: f(x|mu) ~ N(mu, s.x)

# x : vector of observations from the normal dis
# m.mu: mean of the normal prior
# s.mu: standard deviation of the normal prior
# s.x : standard deviation of the normal distribution
# mu: the mean of the population normal distribution

  mean.x <- mean(x)
  n.x <- length(x)

  if(s.x < 0){
      stop("Standard deviation of x must be greate than zero")
    }

  # prior : theta?

  #likelihood

  likelihood <- exp(-n.x / (2 * s.x ^ 2) * (mean.x - mu) ^ 2)


  # posterior
  pos.var <- (s.mu ^ (-2) + n.x * s.x ^ (-2)) ^ (-1)
  pos.sd <- sqrt(pos.var)
  pos.mean <- pos.var * (m.mu * s.mu ^ (-2) + n.x * mean.x * s.x ^ (-2))




}
