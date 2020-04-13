library(rstan)
rstan_options(auto_write = TRUE)

phi <- 1
y <- MASS::rnegbin(60, 50, phi)
plot(y)

m <- stan_model("jacobian.stan")

# out <- purrr::map_dbl(1:20, function(i) {
#   fit <- sampling(m, chains = 6, iter = 800,
#     data = list(N = length(y), y = y, prior_only = 0, phi_prior = 1, prior_type = 0), control = list(adapt_delta = 0.99))
#   mean(extract(fit)$phi)
# })
# hist(out)

phi_inv_prior <- 1
fit0 <- sampling(m, chains = 4, iter = 2000,
  data = list(N = length(y), y = y, prior_only = 0, phi_prior = phi_inv_prior, prior_type = 0), control = list(adapt_delta = 0.99))
fit1 <- sampling(m, chains = 4, iter = 2000,
  data = list(N = length(y), y = y, prior_only = 0, phi_prior = phi_inv_prior, prior_type = 1), control = list(adapt_delta = 0.99))
fit2 <- sampling(m, chains = 4, iter = 2000,
  data = list(N = length(y), y = y, prior_only = 0, phi_prior = 100, prior_type = 2), control = list(adapt_delta = 0.99))

par(mfrow = c(3, 1))
hist(extract(fit0)$phi, breaks = 70);abline(v = phi, col = "red", lwd = 2)
hist(extract(fit1)$phi, breaks = 70);abline(v = phi, col = "red", lwd = 2)
hist(extract(fit2)$phi, breaks = 70);abline(v = phi, col = "red", lwd = 2)
