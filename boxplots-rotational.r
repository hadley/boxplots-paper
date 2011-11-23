

section <- function(data, theta, width = pi / 12, rotate = T) {
  data_angle <- atan2(data[, 2], data[, 1])
  
  distance <- atan2(sin(data_angle - theta), cos(data_angle - theta))
  in_range <- data[distance > (width / 2), , drop = FALSE]
  
  if (!rotate) return(in_range)
  
  sqrt(rowSums(in_range ^ 2))
}
# plot(mat, pch = 20, col = "grey50")
# pmat <- section(mat, -1 * pi / 4, pi / 2, rotate = F) 
# points(pmat, col = "red")
# pmat2 <- section(mat, pi / 4, pi / 3, rotate = T) 
# segments(pmat[, 1], pmat[, 2], pmat2[, 1], 0)

rotational <- function(mat) {

  angles <- seq(0, 2 * pi, length = 100) 
  slices <- lapply(angles, section, data = mat, width = pi / 2, rotate = T)
  stats <- data.frame(
    angle = angles,
    t(sapply(slices, function(x) boxplot.stats(x)$stats))
  )
  plot(mat, xlab = "", ylab = "", type = "n")
  points(0, 0, col = "red", cex = 3, pch = 20)

  x <- stats$X5 * cos(stats$angle)
  y <- stats$X5 * sin(stats$angle)
  polygon(x, y, col = "grey90", border = NA)

  x <- stats$X4 * cos(stats$angle)
  y <- stats$X4 * sin(stats$angle)
  polygon(x, y, col = "grey70", border = NA)

  x <- stats$X3 * cos(stats$angle)
  y <- stats$X3 * sin(stats$angle)
  lines(x, y)

  x <- stats$X2 * cos(stats$angle)
  y <- stats$X2 * sin(stats$angle)
  polygon(x, y, col = "grey90", border = NA)

  x <- stats$X1 * cos(stats$angle)
  y <- stats$X1 * sin(stats$angle)
  polygon(x, y, col = "white", border = NA)
  
  points(mat)

}
