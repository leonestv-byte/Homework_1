library(rjags)
library(R2jags)
library(rjags)

mixture_prior_model_string <- "
model {
  z ~ dcat(p[])
  mu ~ dnorm(mu_mean[z], 1)
}
"

jags_data <- list(
  # Weights
  p = c(0.25, 0.75),
  # Means
  mu_mean = c(0, 3)
)
# Both have standard deviations of 1

set.seed(123)

# Define the Model
mod <- jags.model(file = textConnection(mixture_prior_model_string), data = jags_data, n.chains = 1)


# Discard the First 50K
update(mod, n.iter = 50000)

# Store The Last 50K
samples <- coda.samples(model = mod, variable.names = "mu", n.iter = 50000)

mu_draws <- as.numeric(samples[[1]][, "mu"])

# Plots
x_grid <- seq(min(mu_draws), max(mu_draws), length = 500)

# Plot the Density
plot(density(mu_draws))


# Plot Histogram VS Theoretical
hist(mu_draws, breaks = 60, probability = TRUE, main = "Histogram vs. Theoretical Curve", xlab = "mu", col = "lightgray", border = "white")

y_theoretical <- 0.25 * dnorm(x_grid, mean = 0, sd = 1) + 0.75 * dnorm(x_grid, mean = 3, sd = 1)


lines(x_grid, y_theoretical, col = "red", lwd = 2.5)



