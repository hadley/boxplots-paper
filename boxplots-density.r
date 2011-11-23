library(Hmisc)
library(SuppDists)
library(UsingR)
library(beanplot)
library(hdrcde)
library(denstrip)
source("boxplots-vase.r")

set.seed(141079)

# Generate sample data -------------------------------------------------------

findParams <- function(mu, sigma, skew, kurt) {
  value <- .C("JohnsonMomentFitR", as.double(mu), as.double(sigma), 
    as.double(skew), as.double(kurt - 3), gamma = double(1), 
    delta = double(1), xi = double(1), lambda = double(1), 
    type = integer(1), PACKAGE = "SuppDists")

  list(gamma = value$gamma, delta = value$delta, 
    xi = value$xi, lambda = value$lambda, 
    type = c("SN", "SL", "SU", "SB")[value$type])  
}

# normal
n <- rnorm(100)
# right-skew
s <- rJohnson(100, findParams(0, 1, 2.2, 13.1))
# leptikurtic
k <- rJohnson(100, findParams(0, 1, 0, 20))
# mixture
mm <- rnorm(100, rep(c(-1, 1), each = 50) * sqrt(0.9), sqrt(0.1))

four <- data.frame(
  dist = factor(rep(c("n", "s", "k", "mm"), each = 100),
    c("n", "s", "k", "mm")),
  x = c(n, s, k, mm)
)

pdf("images/four-box.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
boxplot(x ~ dist, data = four) 
dev.off()

pdf("images/four-bean.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
beanplot(x ~ dist, data = four) 
dev.off()

pdf("images/four-violin.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
simple.violinplot(x ~ dist, data = four, bw = 0.2) 
box()
dev.off()

pdf("images/four-vase.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
vase(split(four$x, four$dist), bw = rep(0.2, 4))
axis(side = 2)
dev.off()

pdf("images/four-hdr.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
hdr.boxplot(split(four$x, four$dist), h = 0.2)
dev.off()

pdf("images/four-bpp.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
bpplot(split(four$x, four$dist), main = "")
dev.off()

# Display variations ---------------------------------------------------------

pieces <- split(four$x, four$dist)

pdf("images/four-denstrip.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
plot(c(0.5, 4.5), range(four$x), type = "n", axes = F, xlab = "", ylab = "")
rect(-10, -10, 10, 10, col = "grey80")
for(i in seq_along(pieces)) {
  denstrip(pieces[[i]], at = i, hor = F, width = 0.8, bw = 0.2)
}
box()
axis(2)
axis(1, at = 1:4, labels = names(pieces))
dev.off()

pdf("images/four-sectioned.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
plot(c(0.5, 4.5), range(four$x), type = "n", axes = F, xlab = "", ylab = "")
rect(-10, -10, 10, 10, col = "grey80")
for(i in seq_along(pieces)) {
  sectioned.density(pieces[[i]], at = i + 0.25, hor = F, width = 0.18, 
    bw = 0.2)
}
box()
axis(2)
axis(1, at = 1:4, labels = names(pieces))
dev.off()
