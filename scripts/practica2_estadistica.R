# Daniel Fuertes
# Oscar Moreno
# Marc Torres

# Sesión 1: Inferencia Estadística

# 3


x0 <- c(7.80, 12.60, 15.96, 12.75, 13.50, 8.25, 8.25, 15.05, 13.83, 20.61, 16.46, 18.00, 15.40, 16.25, 13.39, 25.99, 16.64, 11.03, 16.28, 23.40, 14.81, 17.60, 20.21, 18.04, 18.70, 11.10, 15.60, 8.83, 15.05, 16.15,
15.00, 11.10, 21.08, 28.66, 21.25, 20.47, 25.53, 10.89, 15.01, 11.78, 14.82, 12.17, 14.56, 16.96, 15.50, 14.43, 14.43, 12.71, 31.66, 15.75, 15.75, 11.69, 14.52, 17.35, 22.57, 20.00, 13.80, 13.68, 12.61, 19.00, 24.61, 16.80, 16.72, 23.95, 16.11, 19.41, 13.99, 16.48, 13.20, 13.47, 13.63, 14.76, 16.93, 31.31, 12.81, 21.81, 20.82, 35.77, 15.54, 12.62, 13.91, 21.18, 13.72, 12.00, 19.89, 16.46, 32.70, 22.73, 15.51, 16.26, 28.70, 18.90, 25.75, 16.89, 13.99, 13.99, 28.22, 20.79, 16.81, 20.25, 22.31, 24.03, 15.65, 27.28, 12.60, 17.55, 25.60, 29.44, 27.44, 24.00, 30.10, 10.14, 11.31, 11.20, 15.73, 16.90, 25.40, 18.56, 17.55, 21.75, 21.68, 24.60, 46.32, 42.12, 21.70, 21.42, 22.31, 31.35, 10.00, 10.92, 15.72, 17.10, 15.39, 14.79, 14.11, 14.36, 14.53, 15.11, 18.00, 12.08, 12.53, 17.32, 23.96, 22.60, 6.21, 14.54, 16.59, 16.59, 18.26, 19.42, 10.66, 16.01, 16.14, 16.00, 10.15, 16.46, 16.20, 14.28, 12.10, 8.40, 11.10, 7.52, 13.44, 12.48, 14.72, 8.58, 14.87, 14.35, 13.12, 15.40, 15.00, 15.91, 15.20, 14.51, 22.36, 29.29, 22.70, 10.06, 13.89, 14.24, 17.64, 14.62, 16.23, 14.60, 15.99, 13.65, 15.69, 22.44, 7.14, 6.76, 11.02, 10.65, 12.00, 12.96, 10.71, 11.38, 12.63, 11.11, 11.50, 15.61, 14.80, 15.50, 14.51, 14.95, 13.57, 14.80, 11.84, 13.87, 14.43, 16.83, 14.43, 21.12, 22.62, 12.75, 13.92, 11.05, 10.56, 13.32, 22.75, 12.68, 17.75)

hist(x0, breaks = 30)
length(x0)
x1 <- c(x0 <= 8)
sum(x1) # Number of elements in x0 that are <= 8

sum(x1)/length(x0)
# 4

mu <- 16.81
n <- 7
sigma <- 5.91/sqrt(n)

x_obs <- 15.12

x <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 1000)
y <- dnorm(x, mean = mu, sd = sigma)

plot(x, y, type = "l", lwd = 2,
	main = "Normal distribution (mu = 16.81, variance = 4.99)",
	xlab = "X", ylab = "Density")
shade_x <- seq(mu - 4 * sigma, x_obs, length.out = 500)
polygon(c(shade_x, rev(shade_x)),
	   c(dnorm(shade_x, mean = mu, sd = sigma), rep(0, length(shade_x))),
	   col = "lightblue", border = NA)
lines(x, y, lwd = 2)
abline(v = x_obs, col = "red", lwd = 2, lty = 2)

# z = (15.12 - 16.81) / (5.91 / sqrt(7)) ≈ -0.757
z_value <- (x_obs - mu) / sigma

pnorm(z_value) # Lower-tail p-value: P(X <= 15.12)

# 5

n <- 5
sigma <- 5.91/sqrt(n)

x <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 1000)
y <- dnorm(x, mean = mu, sd = sigma)

x2 <- c(13.57,14.8,22.36,22.7,29.29)
mean(x2)


plot(x, y, type = "l", lwd = 2,
	main = "Normal distribution (mu = 16.81, variance = 4.99)",
	xlab = "X", ylab = "Density")
shade_x <- seq(mean(x2), mu + 4 * sigma, length.out = 500)
polygon(c(shade_x, rev(shade_x)),
	   c(dnorm(shade_x, mean = mu, sd = sigma), rep(0, length(shade_x))),
	   col = "lightblue", border = NA)
lines(x, y, lwd = 2)
abline(v = mean(x2), col = "red", lwd = 2, lty = 2)

z2_value <- (mean(x2)-mu) / sigma
pnorm(z2_value, lower.tail = FALSE)

# 6

# 7

p_T <- 0.02
n <- 2300
successes <- 94
p_hat <- successes / n

94/2300
# H0: p_hat - p_T = 0; H1: p_hat - p_T > 0
variance_hat <- p_T * (1 - p_T) / n
se_hat <- sqrt(variance_hat)
z_value <- (p_hat - p_T) / se_hat
p_value <- pnorm(z_value, lower.tail = FALSE)

p_hat
variance_hat
z_value
p_value

# 8

n1 <- 2300
p1 <- 94/n1
v1 <- (p1 * (1 - p1))/n1

n2 <- 1000
p2 <- 56/n2
v2 <- (p2 * (1 - p2))/n2

mu12 <- p2-p1
sd12 <- sqrt(v1 + v2)

z12 <- (p2-p1)/ sqrt(v1 + v2)
z12
p_value12 <- pnorm(z12, lower.tail = FALSE)
p_value12

# Standard normal distribution for the one-sided test (H1: p2 > p1).
z_axis <- seq(-4, 4, length.out = 1000)
z_density <- dnorm(z_axis)
plot(z_axis, z_density, type = "l", lwd = 2,
	main = "Standard normal test statistic",
	xlab = "z", ylab = "Density")

# Shade the upper-tail area corresponding to the p-value.
shade_z <- seq(z12, 4, length.out = 300)
polygon(c(z12, shade_z, 4),
	c(0, dnorm(shade_z), 0),
	col = "lightblue", border = NA)
lines(z_axis, z_density, lwd = 2)
abline(v = z12, col = "red", lwd = 2, lty = 2)
points(z12, dnorm(z12), col = "red", pch = 19)
text(z12, dnorm(z12), labels = sprintf("z = %.2f", z12),
	pos = 4, col = "red")
#text(3.1, 0.18, labels = sprintf("p-value = %.4f", p_value12))


# 9

# 10
