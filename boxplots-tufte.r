tufte <- function(x,..., names = NULL){

  all.x <- list(x, ...)
  n <- length(all.x)
  centers <- seq(from = 0, by = 1.2, length = n)
  ymax <- max(sapply(all.x, max, na.rm = TRUE))
  ymin <- min(sapply(all.x, min, na.rm = TRUE))
  xmax <- max(centers) + 0.5
  xmin <- -0.5
  plot(c(xmin, xmax), c(ymin, ymax), type = "n", main = "", 
          xlab = "", ylab = "", xaxt = "n", yaxt = "n")

  for(i in 1:n) {
    lower <- quantile(all.x[[i]], probs = .25)
    upper <- quantile(all.x[[i]], probs = .75)
    Uextreme <- quantile(all.x[[i]], probs = .99)
    Lextreme <- quantile(all.x[[i]], probs = 0.01)
    Hspread <- (upper - lower)[[1]]
    step <- 1.5*Hspread[[1]]

    if(any(all.x[[i]] > step) | any(all.x[[i]] < -step) ){
      Uextreme <- step; Lextreme <- -step  
      segments(centers[i], Lextreme, centers[i], lower)
      segments(centers[i], Uextreme, centers[i], upper)
  
      count1 = length( all.x[[i]][all.x[[i]] > Uextreme] )
      count2 = length( all.x[[i]][all.x[[i]] < Lextreme] )
      if(count1 > 0) {
        for(j in 1:count1) {
        points(centers[i], all.x[[i]][all.x[[i]] > Uextreme][j] ,
            cex = 0.5)
        }
      }
      if(count2 > 0) {
        for(k in 1:count2) {
        points(centers[i], all.x[[i]][all.x[[i]] < Lextreme][k] ,
            cex = 0.5 )
        }
      }
    } else {
      segments(centers[i], Lextreme, centers[i], lower)
    }
    segments(centers[i], Uextreme, centers[i], upper)
    points(centers[i], median(all.x[[i]]) , pch = 19)
    axis(1, at = centers, labels = names, tick = F)
  }
}

