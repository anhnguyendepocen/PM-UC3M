
# Sample size
n <- 200

## Linearity

png("poisdiagnostics1.png", width = 10, height = 15, res = 200, units = "in")
par(mfcol = c(3, 2), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
fam <- "poisson"
rs <- function(n, eta) rpois(n, lambda = exp(eta))
set.seed(1233132)
# Good
x <- rnorm(n, 0, 1)
y <- rs(n, eta = 1 + 0.5 * x)
plot(glm(y ~ x, family = fam), 1)
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
y <- rs(n, eta = 2 * x)
plot(glm(y ~ x, family = fam), 1)
x <- rpois(n, 10)
y <- rs(n, eta = -1 + x)
plot(glm(y ~ x, family = fam), 1)
# Bad
x <- rnorm(n, 0, 1)
y <- rs(n, eta = 1 + 0.5 * x^2)
plot(glm(y ~ x, family = fam), 1)
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
y <- rs(n, eta = x - sin(x))
plot(glm(y ~ x, family = fam), 1)
x <- rpois(n, 5)
y <- rs(n, eta = x + 0.1 * x^3)
plot(glm(y ~ x, family = fam), 1)
dev.off()

png("logdiagnostics1.png", width = 10, height = 15, res = 200, units = "in")
par(mfcol = c(3, 2), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
fam <- "binomial"
rs <- function(n, eta) rbinom(n, prob = 1 / (1 + exp(-eta)), size = 1)
set.seed(1233132)
# Good
x <- rnorm(n, 0, 1)
y <- rs(n, eta = 1 + 0.5 * x)
plot(glm(y ~ x, family = fam), 1)
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
y <- rs(n, eta = 2 * x)
plot(glm(y ~ x, family = fam), 1)
x <- rpois(n, 10)
y <- rs(n, eta = -1 + x)
plot(glm(y ~ x, family = fam), 1)
# Bad
x <- rnorm(n, 0, 1)
y <- rs(n, eta = 1 + x^2)
plot(glm(y ~ x, family = fam), 1)
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
y <- rs(n, eta = sin(x))
plot(glm(y ~ x, family = fam), 1)
x <- rpois(n, 5)
y <- rs(n, eta = x - 0.5 * x^2)
plot(glm(y ~ x, family = fam), 1)
dev.off()

## Distribution response

png("poisdiagnostics2.png", width = 10, height = 15, res = 200, units = "in")
par(mfcol = c(3, 2), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
fam <- "poisson"
rs <- function(n, eta) rpois(n, lambda = exp(eta))
set.seed(1233132)
# Good
x <- rnorm(n, 0, 1)
y <- rs(n, eta = 1 + 0.5 * x)
plot(glm(y ~ x, family = fam), 2)
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
y <- rs(n, eta = 2 * x)
plot(glm(y ~ x, family = fam), 2)
x <- rpois(n, 10)
y <- rs(n, eta = -1 + x)
plot(glm(y ~ x, family = fam), 2)
# Bad
x <- rnorm(n, 0, 1)
y <- rs(n, eta = 1 + 0.5 * x^2)
plot(glm(y ~ x, family = fam), 2)
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
y <- rs(n, eta = x - sin(x))
plot(glm(y ~ x, family = fam), 2)
x <- rpois(n, 5)
y <- rs(n, eta = x + 0.1 * x^3)
plot(glm(y ~ x, family = fam), 2)
dev.off()

png("logdiagnostics2.png", width = 10, height = 15, res = 200, units = "in")
par(mfcol = c(3, 2), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
fam <- "binomial"
rs <- function(n, eta) rbinom(n, prob = 1 / (1 + exp(-eta)), size = 1)
set.seed(1233132)
# Good
x <- rnorm(n, 0, 1)
y <- rs(n, eta = 1 + 0.5 * x)
plot(glm(y ~ x, family = fam), 2)
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
y <- rs(n, eta = 2 * x)
plot(glm(y ~ x, family = fam), 2)
x <- rpois(n, 10)
y <- rs(n, eta = -1 + x)
plot(glm(y ~ x, family = fam), 2)
# Bad
x <- rnorm(n, 0, 1)
y <- rs(n, eta = 1 + x^2)
plot(glm(y ~ x, family = fam), 2)
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
y <- rs(n, eta = sin(x))
plot(glm(y ~ x, family = fam), 2)
x <- rpois(n, 5)
y <- rs(n, eta = x - 0.5 * x^2)
plot(glm(y ~ x, family = fam), 2)
dev.off()

## Independence

png("poisdiagnostics3.png", width = 10, height = 15, res = 200, units = "in")
par(mfcol = c(3, 2), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
fam <- "poisson"
rs <- function(n, eta) rpois(n, lambda = exp(eta))
set.seed(1233132)
# Good
x <- rnorm(n, 0, 1)
y <- rs(n, eta = 1 + 0.5 * x)
plot(glm(y ~ x)$residuals, type = "o", ylab = "Residuals")
x <- c(rnorm(n/2, 1, 0.5), rnorm(n/2, -1, 0.5))
y <- rs(n, eta = 2 * x)
plot(glm(y ~ x)$residuals, type = "o", ylab = "Residuals")
x <- rpois(n, 5)
y <- rs(n, eta = -1 + 0.25 * x)
plot(glm(y ~ x)$residuals, type = "o", ylab = "Residuals")
# Bad
x <- rnorm(n, 0, 1)
y <- rs(n, eta = 1 + 0.5 * x)
y <- rep(y, each = 5)[1:n] # Repeats observations
plot(glm(y ~ x)$residuals, type = "o", ylab = "Residuals")
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
y <- 3
for (i in 2:n) y[i] <- rpois(1, lambda = y[i - 1] + 1) # Positive correlation
plot(glm(y ~ x)$residuals, type = "o", ylab = "Residuals")
x <- rpois(n, 5)
y <- 1
for (i in 2:n) y[i] <- rpois(1, lambda = 1 / (y[i - 1] + 1)) # Negative correlation
plot(glm(y ~ x)$residuals, type = "o", ylab = "Residuals")
dev.off()

png("logdiagnostics3.png", width = 10, height = 15, res = 200, units = "in")
par(mfcol = c(3, 2), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
fam <- "binomial"
rs <- function(n, eta) rbinom(n, prob = 1 / (1 + exp(-eta)), size = 1)
set.seed(1233132)
# Good
x <- rnorm(n, 0, 1)
y <- rs(n, eta = 1 + 0.5 * x)
plot(glm(y ~ x)$residuals, type = "o", ylab = "Residuals")
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
y <- rs(n, eta = 2 * x)
plot(glm(y ~ x)$residuals, type = "o", ylab = "Residuals")
x <- rpois(n, 10)
y <- rs(n, eta = -1 + x)
plot(glm(y ~ x)$residuals, type = "o", ylab = "Residuals")
# Bad
x <- rnorm(n, 0, 1)
y <- rs(n, eta = 1 + 0.5 * x)
y <- rep(y, each = 5)[1:n] # Repeats observations
plot(glm(y ~ x)$residuals, type = "o", ylab = "Residuals")
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
y <- 1
for (i in 2:n) y[i] <- rbinom(1, size = 1, prob = 0.1 + 0.75 * y[i - 1]) # Positive correlation
plot(glm(y ~ x)$residuals, type = "o", ylab = "Residuals")
x <- rpois(n, 5)
y <- 1
for (i in 2:n) y[i] <- rbinom(1, size = 1, prob = 1 - 0.75 * y[i - 1]) # Negative correlation
plot(glm(y ~ x)$residuals, type = "o", ylab = "Residuals")
dev.off()
