library(MASS)
# Generate mixture of 2 correlated normals
set.seed(141079)

mat <- rbind(
  mvrnorm(150, c(1, 1), matrix(c(1, 0.9, 0.9, 1), nrow = 2, byrow = T)),
  mvrnorm(150, c(0, 3), matrix(c(0.5, 0, 0, 0.5), nrow = 2, byrow = T))
)

mat <- scale(mat, scale = F)

plot(mat)
points(0, 0, col = "red", cex = 3, pch = 20)

proj <- function(data, theta) {
  pmat <- matrix(c(cos(theta), -sin(theta), 
           sin(theta), cos(theta)), byrow = T, nrow = 2)
  data %*% pmat
}

section <- function(data, theta, width = pi / 12, rotate = T) {
  data_angle <- atan2(data[, 2], data[, 1])
  
  distance <- atan2(sin(data_angle - theta), cos(data_angle - theta))
  in_range <- data[distance > (width / 2), , drop = FALSE]
  
  if (!rotate) return(in_range)
  
  sqrt(rowSums(in_range ^ 2))
}


# Clockwise bivariate bag plot -----------------------------------------------
plot(mat, type = "n")
pmat <- proj(mat, pi / 12) 
points(pmat, col = "red")
arrows(mat[, 1], mat[, 2], pmat[, 1], pmat[, 2], length = 0.1)


# Only need to go to pi because have upper and lower fourths
angles <- seq(0, pi, length = 100) 
projs <- lapply(angles, proj, data = mat)

stats <- data.frame(
  angle = angles,
  t(sapply(projs, function(x) boxplot.stats(x[, 1])$stats))
)

pdf("images/2d-clockwise.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
plot(mat, xlab = "", ylab = "")
points(0, 0, col = "red", cex = 3, pch = 20)

x <- c(stats$X2, stats$X4) * cos(stats$angle)
y <- c(stats$X2, stats$X4) * sin(stats$angle)
lines(x, y)
# ch <- chull(x, y)
# lines(x[ch], y[ch])

x <- c(stats$X1, stats$X5) * cos(stats$angle)
y <- c(stats$X1, stats$X5) * sin(stats$angle)
lines(x, y)
# ch <- chull(x, y)
# ch <- c(ch, ch[1])
# lines(x[ch], y[ch])
dev.off()

# Rotational boxplot ---------------------------------------------------------
plot(mat, pch = 20, col = "grey50")
pmat <- section(mat, -1 * pi / 4, pi / 2, rotate = F) 
points(pmat, col = "red")

pmat2 <- section(mat, pi / 4, pi / 3, rotate = T) 
# segments(pmat[, 1], pmat[, 2], pmat2[, 1], 0)

angles <- seq(0, 2 * pi, length = 100) 
slices <- lapply(angles, section, data = mat, width = pi / 2, rotate = T)
stats <- data.frame(
  angle = angles,
  t(sapply(slices, function(x) boxplot.stats(x)$stats))
)

pdf("images/2d-rotational.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
plot(mat, xlab = "", ylab = "")
points(0, 0, col = "red", cex = 3, pch = 20)

x <- stats$X1 * cos(stats$angle)
y <- stats$X1 * sin(stats$angle)
lines(x, y, col = "grey50")

x <- stats$X2 * cos(stats$angle)
y <- stats$X2 * sin(stats$angle)
lines(x, y)

x <- stats$X3 * cos(stats$angle)
y <- stats$X3 * sin(stats$angle)
lines(x, y, lwd = 2)

x <- stats$X4 * cos(stats$angle)
y <- stats$X4 * sin(stats$angle)
lines(x, y)

x <- stats$X5 * cos(stats$angle)
y <- stats$X5 * sin(stats$angle)
lines(x, y, col = "grey50")

dev.off()
