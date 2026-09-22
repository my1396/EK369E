# initialize parameters
mu.A = 0.15
sig.A = 0.1
sig2.A = sig.A^2

mu.B = 0.12
sig.B = 0.07
sig2.B = sig.B^2

# rho.AB = -0.164
# sig.AB = rho.AB * sig.A * sig.B
sig.AB = 0.002

rf <- .03 # risk-free rate

# IntroCompFinR takes the expected returns as a named vector and the
# covariances as a named matrix
mu.vec <- c(mu.A, mu.B)
names(mu.vec) <- c("Asset A", "Asset B")

sigma.vec <- c(sig.A, sig.B)
names(sigma.vec) <- c("Asset A", "Asset B")

sigma2.mat <- matrix(c(sig2.A, sig.AB, sig.AB, sig2.B), ncol=2)
colnames(sigma2.mat) <- rownames(sigma2.mat) <- c("Asset A", "Asset B")

# Global minimum variance portfolio
gmv.port <- globalMin.portfolio(mu.vec, sigma2.mat)
gmv.port

# Tangency portfolio
tan.port <- tangency.portfolio(mu.vec, sigma2.mat, risk.free = rf)
tan.port
cat("Sharpe ratio of Tangency portfolio:\n")
((tan.port$er - rf) / tan.port$sd) %>% round(4)
