#' @title Generate samples from Rayleigh distribution
#' @name Rayleigh.1
#' @description Generate samples from Rayleigh distribution
#' @param x vector of quantiles
#' @param sigma the parameter of Rayleigh density
#' @importFrom stats runif
#' @return a value
#' @examples
#' \dontrun{
#' Rayleigh.1(10,1)
#' }
#' @export
Rayleigh.1 <- function(x, sigma) {
  m=1000
  u <- runif(m/2)
  v <- runif(m/2)
  u <- c(u, v)
  cdf <- numeric(length(x))
  for (i in 1:length(x)) {
    g <- x[i]^2/sigma^2 * u * exp(-x[i]^2/(2 * sigma^2) * u^2)
    cdf[i] <- mean(g)
  }
  return(cdf)
}
