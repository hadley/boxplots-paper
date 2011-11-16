quelplot <- function(x,y, xlim = NULL, ylim = NULL, xlab = "", ylab = "", D = 5) {
  Tx <- mean(x)
  Ty <- mean(y)
  n <- length(x)

  # Need robust estimators for Sx, Sy and R
  Sx <- sd(x) 
  Sy <- sd(y)
  R <- cor(x, y)
  coef <- sign(R)

  # 2.2 The quelplot
  
  Xs <- (x - Tx) / Sx
  Ys <- (y - Ty) / Sy
  
  Z1 <- (Ys + Xs) / sqrt(2 * (1 + R))
  Z2 <- (Ys - Xs) / sqrt(2 * (1 - R))
  
  # 3.2 Estimation of asymmetry parameters
  Zprime <- function(Z) {
    function(x) {
      p <- x[1]
      z <- x[2]
      Zprime <- ifelse(Z < z, p, (1 - p)) * (Z - z)
      abs(sum(Zprime)) + abs(sum(Zprime ^ 2 * sign(Zprime)))
    }
  }
  
  asym1 <- optim(c(0.5, mean(Z1)), Zprime(Z1))
  asym2 <- optim(c(0.5, mean(Z1)), Zprime(Z1))
  
  P1 <- asym1$par[1]
  P2 <- asym2$par[1]

  F1 <- Z1 / ifelse(Z1 > 0, 2 * P1, 2 * (1 - P1))
  F2 <- Z2 / ifelse(Z2 > 0, 2 * P2, 2 * (1 - P2))

  E <- sqrt(F1 ^ 2 + F2 ^ 2)
  W <- (1 - E^2 / 36) ^ 2 # 36 = C, chosen based on simulation
  
  Em <- median(E)
  Emax <- max(E[E^2 < D * Em^2]) # Constant ratio (full quel) 
  
  # Plot points
  if(is.null(xlim)) xlim <- c(min(x) - 2 * sd(x), max(x) + 2 * sd(x))
  if(is.null(ylim)) ylim <- c(min(y) - 2 * sd(y), max(y) + 2 * sd(y))
  
  plot(x, y, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab)

  # Appendix A: construction of quelplots
  theta <- seq(0, 2 * pi, length = 100)

  # Hinge
  R1neg <- 2 * (1 - P1) * Em * sqrt((1 + R) / 2)
  R1pos <- 2 * P1       * Em * sqrt((1 + R) / 2)
  R2neg <- 2 * (1 - P2) * Em * sqrt((1 - R) / 2)
  R2pos <- 2 * P2       * Em * sqrt((1 - R) / 2)

  theta_1 <- ifelse(cos(theta) > 0, R1pos, R1neg) * cos(theta)
  theta_2 <- ifelse(sin(theta) > 0, R2pos, R2neg) * sin(theta)
  
  X <- Tx + (theta_1 + theta_2) * Sx
  Y <- Ty + (theta_1 - theta_2) * Sy

  lines(X, Y, lwd = 2)

  # Fence
  R1neg <- 2 * (1 - P1) * Emax * sqrt((1 + R) / 2)
  R1pos <- 2 * P1       * Emax * sqrt((1 + R) / 2)
  R2neg <- 2 * (1 - P2) * Emax * sqrt((1 - R) / 2)
  R2pos <- 2 * P2       * Emax * sqrt((1 - R) / 2)

  theta_1 <- ifelse(cos(theta) > 0, R1pos, R1neg) * cos(theta)
  theta_2 <- ifelse(sin(theta) > 0, R2pos, R2neg) * sin(theta)
  
  X <- Tx + (theta_1 + theta_2) * Sx
  Y <- Ty + (theta_1 - theta_2) * Sy

  lines(X, Y, col = "grey50", lwd = 2)
  
  # Regression lines
  x_grid <- seq(min(x), max(x), length = 2)
  y_pred <- Ty + (x_grid - Tx) * R * Sy / Sx

  y_grid <- seq(min(y), max(y), length = 2)
  x_pred <- Tx + (y_grid - Ty) * R * Sx / Sy
  
  lines(x_grid, y_pred)
  lines(x_pred, y_grid)
  
}

# set.seed(1410)
# quelplot(rnorm(100), rnorm(100))
