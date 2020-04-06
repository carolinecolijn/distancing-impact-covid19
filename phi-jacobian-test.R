library(rstan)
rstan_options(auto_write = TRUE)
wd <- getwd()
setwd(here::here("selfIsolationModel", "stan"))

fit <- stan("phi-jacobian-test.stan")
print(fit)

fit2 <- stan("phi-jacobian-test2.stan")
print(fit2)

setwd(wd)
#
# inv_sqrt_phi <- rnorm(1e3, 0, 1)
# inv_sqrt_phi <- inv_sqrt_phi[inv_sqrt_phi>0]
#
# inv_sqrt_phi_star <- - log(0.5 * inv_sqrt_phi^-0.5/sqrt(inv_sqrt_phi)^2)
#
# inv_sqrt_phi = 1/sqrt(phi)
# sqrt(phi) * inv_sqrt_phi = 1
# sqrt(phi) = 1 / inv_sqrt_phi
# phi = (1 / inv_sqrt_phi)^2
#
# hist((1 / inv_sqrt_phi)^2)
# hist((1 / inv_sqrt_phi_star)^2)

inv_sqrt_phi <- rnorm(1e3, 0, 1)
inv_sqrt_phi <- inv_sqrt_phi[inv_sqrt_phi>0]
hist(inv_sqrt_phi)
# hist(exp(log(1/inv_sqrt_phi) - log(1/inv_sqrt_phi^2)))

mean(log(inv_sqrt_phi))
mean(log(1/inv_sqrt_phi))
mean(log(1/inv_sqrt_phi) - log(1/inv_sqrt_phi^2))
mean(log(1/inv_sqrt_phi) + log(inv_sqrt_phi^2))
mean(log(1/inv_sqrt_phi) + 2 * log(inv_sqrt_phi))

inv_phi <- rnorm(1e3, 0, 1)
inv_phi <- inv_phi[inv_phi>0]
phi <- 1/inv_phi
mean(phi)
mean(phi - log(1/phi^2))

mean(log(1/inv_sqrt_phi) + log(inv_sqrt_phi^2))
mean(log(1/inv_sqrt_phi) + 2 * log(inv_sqrt_phi))

mean(log(1/sqrt(inv_sqrt_phi)) + log(0.5 * inv_sqrt_phi^-0.5/sqrt(inv_sqrt_phi)^2))

#
# inv_sqrt_phi_star <-  log(0.5 * inv_sqrt_phi^-0.5/sqrt(inv_sqrt_phi)^2)
# hist((1 / inv_sqrt_phi_star)^2)
#
# foo <- function(x, shape1, shape2) dbeta(plogis(x), shape1, shape2) * dlogis(x)
# integrate(foo, lower = -Inf, upper = Inf, shape1 = sqrt(2), shape2 = pi) # 1.00000

x <- 4.6
log(1/x^2)
-log(x^2)
-2 * log(x)

# phi <- 3.6
# log(0.5 * phi^-0.5/sqrt(phi)^2)
