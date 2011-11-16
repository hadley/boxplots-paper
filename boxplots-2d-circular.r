library(MASS)
source("boxplots-rotational.r")
source("boxplots-clockwise.r")

# Generate mixture of 2 correlated normals
set.seed(141079)

mat <- rbind(
  mvrnorm(150, c(1, 1), matrix(c(1, 0.9, 0.9, 1), nrow = 2, byrow = T)),
  mvrnorm(150, c(0, 3), matrix(c(0.5, 0, 0, 0.5), nrow = 2, byrow = T))
)

mat <- scale(mat, scale = F)



pdf("images/2d-clockwise.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
clockwise(mat)
dev.off()

# Rotational boxplot ---------------------------------------------------------


pdf("images/2d-rotational.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))

rotational(mat)

dev.off()
