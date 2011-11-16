library(MASS)
library(hdrcde)
source("boxplots-relplot.r")
source("boxplots-quelplot.r")

# Generate mixture of 2 correlated normals
set.seed(141079)

mat <- rbind(
  mvrnorm(150, c(1, 1), matrix(c(1, 0.9, 0.9, 1), nrow = 2, byrow = T)),
  mvrnorm(150, c(2, 4), matrix(c(1, 0.9, 0.9, 1), nrow = 2, byrow = T))
)
mat <- rbind(
  mvrnorm(150, c(1, 1), matrix(c(1, 0.9, 0.9, 1), nrow = 2, byrow = T)),
  mvrnorm(150, c(0, 3), matrix(c(0.5, 0, 0, 0.5), nrow = 2, byrow = T))
)


# Range-finder boxplot -------------------------------------------------------

x_stats <- boxplot(mat[, 1], plot = F)$stats
y_stats <- boxplot(mat[, 2], plot = F)$stats

pdf("images/2d-rangefinder.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))

plot(mat, xlab = "", ylab = "")

points(x_stats[3], y_stats[3], col = "grey50", pch = 20, cex = 2)

# The cross
segments(x_stats[2], y_stats[3], x_stats[4], y_stats[3], 
  col = "grey50", lwd = 2)
segments(x_stats[3], y_stats[2], x_stats[3], y_stats[4], 
  col = "grey50", lwd = 2)

# Outer ranges
segments(x_stats[2], y_stats[1], x_stats[4], y_stats[1], 
  col = "grey50", lwd = 2)
segments(x_stats[2], y_stats[5], x_stats[4], y_stats[5], 
  col = "grey50", lwd = 2)

segments(x_stats[1], y_stats[2], x_stats[1], y_stats[4], 
  col = "grey50", lwd = 2)
segments(x_stats[5], y_stats[2], x_stats[5], y_stats[4], 
  col = "grey50", lwd = 2)

dev.off()

# 2d plots -------------------------------------------------------------------

pdf("images/2d-relplot.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))

relplot(mat[, 1], mat[, 2], xlab = "", ylab = "")
dev.off()

pdf("images/2d-quelplot.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))

quelplot(mat[, 1], mat[, 2], xlim = range(mat[, 1]), ylim = range(mat[, 2]))
dev.off()

pdf("images/2d-bagplot.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
bp <- aplpack::compute.bagplot(mat[, 1], mat[, 2])
plot(mat, xlab = "", ylab = "", type = "n")
polygon(bp$hull.bag, col = "grey80")
polygon(bp$hull.loop)
points(mat)

dev.off()

pdf("images/2d-hdr-5.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
hdr.boxplot.2d(mat[, 1], mat[, 2], h = rep(5, 2))
dev.off()

pdf("images/2d-hdr-2-5.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
hdr.boxplot.2d(mat[, 1], mat[, 2], h = rep(2.5, 2))
dev.off()

pdf("images/2d-hdr-1.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
hdr.boxplot.2d(mat[, 1], mat[, 2], h = rep(1, 2))
dev.off()
