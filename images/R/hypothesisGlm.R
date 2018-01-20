
# Plotting function
n <- 200
plotFun <- function(x, eta, y, n = 200, ind = 1, fam = "poisson") {
  
  if (missing(y)) {
    y <- switch(fam,
      "poisson" = rpois(n, lambda = exp(eta)),
      "binomial" = rbinom(n, prob = 1 / (1 + exp(-eta)), size = 1))
  }
  mod <- glm(y ~ x, family = fam)
  plot(x, y, pch = 16, main = "Data scatterplot")
  t <- seq(-100, 100, l = 1e4)
  lines(t, predict(mod, newdata = data.frame(x = t), type = "response"), 
        col = 2)
  if (ind < 7) {
    plot(mod, which = ind,
         main = list("Residuals vs Fitted", "Normal Q-Q", "Scale-Location", 
                     "Cook's distance", "Residuals vs Leverage", 
                     expression("Cook's dist vs Leverage  " 
                                * h[ii]/(1 - h[ii])))[ind], caption = "")
  } else {
    plot(mod$residuals, type = "o", ylab = "Residuals", 
         main = "Residuals series")
  }
  
}

## Linearity

# Poisson

# Good
png("poisdiag1-good.png", width = 15, height = 10, res = 200, units = "in")
par(mfcol = c(2, 3), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
set.seed(1233132)
x <- rnorm(n, 0, 1)
plotFun(x = x, eta = 1 + 0.5 * x, n = n, ind = 1, fam = "poisson")
x <- sample(c(rnorm(n, 3, 0.5), rnorm(n, -3, 0.5)), size = n)
plotFun(x = x, eta = -0.5 * x, n = n, ind = 1, fam = "poisson")
x <- rpois(n, 5)
plotFun(x = x, eta = -3 + 0.5 * x, n = n, ind = 1, fam = "poisson")
dev.off()

# Bad
png("poisdiag1-bad.png", width = 15, height = 10, res = 200, units = "in")
par(mfcol = c(2, 3), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
set.seed(1233132)
x <- rnorm(n, 0, 1)
plotFun(x = x, eta = 1 + 0.25 * x^2, n = n, ind = 1, fam = "poisson")
x <- sample(c(rnorm(n, 3, 0.5), rnorm(n, -3, 0.5)), size = n)
plotFun(x = x, eta = 0.5 * (x - sin(x)), n = n, ind = 1, fam = "poisson")
x <- rpois(n, 5)
plotFun(x = x, eta = 5 - 0.1 * x - 0.1 * x^2, n = n, ind = 1, fam = "poisson")
dev.off()

# Binomial

# Good
png("logdiag1-good.png", width = 15, height = 10, res = 200, units = "in")
par(mfcol = c(2, 3), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
set.seed(1233132)
x <- rnorm(n, 0, 1)
plotFun(x = x, eta = 1 + 0.5 * x, n = n, ind = 1, fam = "binomial")
x <- sample(c(rnorm(n, 3, 0.5), rnorm(n, -3, 0.5)), size = n)
plotFun(x = x, eta = 1.5 * x, n = n, ind = 1, fam = "binomial")
x <- rpois(n, 10)
plotFun(x = x, eta = -5 + x, n = n, ind = 1, fam = "binomial")
dev.off()

# Bad
png("logdiag1-bad.png", width = 15, height = 10, res = 200, units = "in")
par(mfcol = c(2, 3), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
set.seed(1233132)
x <- rnorm(n, 0, 1)
plotFun(x = x, eta = 1 + x^2, n = n, ind = 1, fam = "binomial")
x <- sample(c(rnorm(n, 3, 0.5), rnorm(n, -3, 0.5)), size = n)
plotFun(x = x, eta = 2 * sin(x), n = n, ind = 1, fam = "binomial")
x <- rpois(n, 10)
plotFun(x = x, eta = 0.1 * (x - 10) * (x - 5) * (x - 20), n = n, ind = 1, fam = "binomial")
dev.off()

## Distribution response

# Poisson

# Good
png("poisdiag2-good.png", width = 15, height = 10, res = 200, units = "in")
par(mfcol = c(2, 3), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
set.seed(1233132)
x <- rnorm(n, 0, 1)
plotFun(x = x, eta = 1 + 0.5 * x, n = n, ind = 2, fam = "poisson")
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
plotFun(x = x, eta = 0.8 * x, n = n, ind = 2, fam = "poisson")
x <- rpois(n, 20)
plotFun(x = x, eta = 1 - 0.1 * x, n = n, ind = 2, fam = "poisson")
dev.off()

# Bad
png("poisdiag2-bad.png", width = 15, height = 10, res = 200, units = "in")
par(mfcol = c(2, 3), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
set.seed(1233132)
x <- rnorm(n, 0, 1)
plotFun(x = x, y = round(rnorm(n, exp(1 + 0.5 * x))), n = n, ind = 2, 
        fam = "poisson")
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
plotFun(x = x, eta = 0.5 * sample(c(x, -x), size = n), n = n, ind = 2,
        fam = "poisson")
x <- rpois(n, 10)
plotFun(x = x, y = rbinom(n = n, size = 5, prob = 1 / (1 + exp(-(-10 + x)))),
        n = n, ind = 2, fam = "poisson")
dev.off()

# Binomial

# Good
png("logdiag2-good.png", width = 15, height = 10, res = 200, units = "in")
par(mfcol = c(2, 3), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
set.seed(1233132)
x <- rnorm(n, 0, 1)
plotFun(x = x, eta = 1 + 0.5 * x, n = n, ind = 2, fam = "binomial")
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
plotFun(x = x, eta = 2 * x, n = n, ind = 2, fam = "binomial")
x <- rpois(n, 10)
plotFun(x = x, eta = -5 + x, n = n, ind = 2, fam = "binomial")
dev.off()

# Bad
png("logdiag2-bad.png", width = 15, height = 10, res = 200, units = "in")
par(mfcol = c(2, 3), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
set.seed(1233132)
x <- rnorm(n, 0, 1)
plotFun(x = x, eta = 1 + x^2, n = n, ind = 2, fam = "binomial")
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
plotFun(x = x, eta = 3 * sin(x), n = n, ind = 2, fam = "binomial")
x <- rpois(n, 5)
plotFun(x = x, eta = x - 0.5 * x^2, n = n, ind = 2, fam = "binomial")
dev.off()

## Independence

# Poisson

# Good
png("poisdiag3-good.png", width = 15, height = 10, res = 200, units = "in")
par(mfcol = c(2, 3), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
set.seed(1233132)
x <- rnorm(n, 0, 1)
plotFun(x = x, eta = 1 + 0.5 * x, n = n, ind = 7, fam = "poisson")
x <- sample(c(rnorm(n, 1, 0.5), rnorm(n, -1, 0.5)), size = n)
plotFun(x = x, eta = 2 * x, n = n, ind = 7, fam = "poisson")
x <- rpois(n, 5)
plotFun(x = x, eta = 1 - 0.25 * x, n = n, ind = 7, fam = "poisson")
dev.off()

# Bad
png("poisdiag3-bad.png", width = 15, height = 10, res = 200, units = "in")
par(mfcol = c(2, 3), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
set.seed(1233132)
x <- rnorm(n, 0, 1)
y <- rpois(n, lambda = exp(1 + 0.5 * x))
y <- rep(y, each = 5)[1:n] # Repeats observations
plotFun(x = x, y = y, n = n, ind = 7, fam = "poisson")
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
y <- 3
for (i in 2:n) y[i] <- rpois(1, lambda = y[i - 1] + 1) 
plotFun(x = x, y = y, n = n, ind = 7, fam = "poisson") # Positive correlation
x <- rpois(n, 5)
y <- 1
for (i in 2:n) y[i] <- rpois(1, lambda = 2 / (y[i - 1] + 1)) 
plotFun(x = x, y = y, n = n, ind = 7, fam = "poisson") # Negative correlation
dev.off()

# Binomial

# Good
png("logdiag3-good.png", width = 15, height = 10, res = 200, units = "in")
par(mfcol = c(2, 3), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
set.seed(1233132)
x <- rnorm(n, 0, 1)
plotFun(x = x, eta = 1 + 0.5 * x, n = n, ind = 7, fam = "binomial")
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
plotFun(x = x, eta = 2 * x, n = n, ind = 7, fam = "binomial")
x <- rpois(n, 10)
plotFun(x = x, eta = -5 + x, n = n, ind = 7, fam = "binomial")
dev.off()

# Bad
png("logdiag3-bad.png", width = 15, height = 10, res = 200, units = "in")
par(mfcol = c(2, 3), mar = c(4, 4, 2, 1) + 0.1, lwd = 2)
set.seed(1233132)
x <- rnorm(n, 0, 1)
y <- rbinom(n, size = 1, prob = 1 + 0.5 * x)
y <- rep(y, each = 5)[1:n] # Repeats observations
plotFun(x = x, y = y, n = n, ind = 7, fam = "binomial")
x <- c(rnorm(n/2, 3, 1), rnorm(n/2, -3, 1))
y <- 1
for (i in 2:n) y[i] <- rbinom(1, size = 1, prob = 0.1 + 0.75 * y[i - 1]) 
plotFun(x = x, y = y, n = n, ind = 7, fam = "binomial") # Positive correlation
x <- rpois(n, 5)
y <- 1
for (i in 2:n) y[i] <- rbinom(1, size = 1, prob = 1 - 0.75 * y[i - 1])
plotFun(x = x, y = y, n = n, ind = 7, fam = "binomial") # Negative correlation
dev.off()
