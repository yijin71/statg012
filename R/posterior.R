
posterior <- function(a, b, c, d, model= "") {
  if ( model == "binbeta") {
   res <- binbeta(a, b, c, d)
  }
  if ( model == "poigamma" ) {
    res <- poigamma(a, b, c, d)
  }
  if (model == "norm"){
    res <- rnorm(a, b, c, d)
  }
  return(res)
}



