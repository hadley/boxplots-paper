# Clockwise bivariate bag plot -----------------------------------------------


proj <- function(data, theta) {
  pmat <- matrix(c(cos(theta), -sin(theta), 
           sin(theta), cos(theta)), byrow = T, nrow = 2)
  data %*% pmat
}

# plot(mat, type = "n")
# pmat <- proj(mat, pi / 12) 
# points(pmat, col = "red")
# arrows(mat[, 1], mat[, 2], pmat[, 1], pmat[, 2], length = 0.1)

clockwise <- function(mat) {
  
  # Only need to go to pi because have upper and lower fourths
  angles <- seq(0, pi, length = 100) 
  projs <- lapply(angles, proj, data = mat)

  stats <- data.frame(
    angle = angles,
    t(sapply(projs, function(x) boxplot.stats(x[, 1])$stats))
  )


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
}
