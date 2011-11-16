library(Hmisc)
library(rpart)
# library(aplpack) #for the bagplot
set.seed(100)

source("boxplots-tufte.r")

data(mpg, package = "ggplot2")

# Variable width boxplot -----------------------------------------------------

freq <- 10 ^ (2:5)
df <- data.frame(
  group = rep(letters[seq_along(freq)], freq), 
  x = rnorm(sum(freq))
)

pdf("images/width-boxplot.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
boxplot(x ~ group, data = df)
dev.off()

pdf("images/width-variable.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
boxplot(x ~ group, data = df, varwidth = TRUE)
dev.off()

pdf("images/width-notched.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
boxplot(x ~ group, data = df, notch = TRUE)
dev.off()

# Letter value boxplot -------------------------------------------------------
source("lvplot/calculate.r")
source("lvplot/draw.r")
source("lvplot/lvplot.r")

pdf("images/letter-value.pdf", width = 4, height = 4)
par(mar = c(2.1, 2.1, .1, .1))
LVboxplot(df$x ~ df$group, horizontal = FALSE)
dev.off()


