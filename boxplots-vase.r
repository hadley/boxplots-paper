vase <- function(x, ..., names = NULL, bw = NULL) {
  
  
  all.x <- c(x, list(...))

  centers <- seq_along(all.x)
  n <- length(all.x)
  if (is.null(names)) {
    names <- names(all.x)
  }

  xmin <- 0.5
  xmax <- length(all.x) + 0.5

  ymin <- min(unlist(all.x), na.rm = TRUE)
  ymax <- max(unlist(all.x), na.rm = TRUE)
  
  plot(c(xmin, xmax), c(ymin, ymax), type = "n", main = "", 
          xlab = "", ylab = "", xaxt = "n", yaxt = "n")

  for(i in 1:n){

    lower <- quantile(all.x[[i]], probs = .25)
    upper <- quantile(all.x[[i]], probs = .75)
    Uex <- quantile(all.x[[i]], probs = .95)
    Lex <- quantile(all.x[[i]], probs = 0.05)
    Hspread <- (upper - lower)[[1]]
    step <- 1.5*Hspread[[1]]


    Xs <- density(all.x[[i]], bw = bw[i])$x
    Ys <- density(all.x[[i]], bw = bw[i])$y

    in_box <- Xs < upper & Xs > lower
    Xs <- c(lower, Xs[in_box], upper)
    Ys <- c(0, Ys[in_box], 0)
    
    # Scale to 0.4
    Ys <- Ys / max(Ys) * 0.4

    has_outliers <- any(all.x[[i]] > upper[[1]]+step) ||
      any(all.x[[i]] < -lower[[1]]-step) 
    if(has_outliers){
      Uex <- Uex; Lex <- Lex  
      segments(centers[i], upper[[1]]+step, centers[i], upper, col = "grey")
      segments(centers[i], -lower[[1]]-step, centers[i], lower, col = "grey")
  
      count1 = length( all.x[[i]][all.x[[i]] > upper[[1]]+step] )
      count2 = length( all.x[[i]][all.x[[i]] < -lower[[1]]-step] )

      if(count1 > 0) {
        for(j in 1:count1){
          points(centers[i], all.x[[i]][all.x[[i]] > upper[[1]]+step][j],
            cex = 0.5 )
        }
      }
      if(count2 > 0){
        for(k in 1:count2){
          points(centers[i], all.x[[i]][all.x[[i]] < -lower[[1]]-step][k],
            cex = 0.5 )
        }
      }
    }

    lines(centers[i]+Ys, Xs)
    lines(centers[i]+Ys, Xs)
    lines(centers[i]-Ys, Xs)
    segments(centers[[i]], Lex, centers[[i]], lower)
    segments(centers[[i]], Uex, centers[[i]], upper)

    ###plot a line for the median
    pos <- which.min(abs(Xs - median(Xs)))[1]
    segments((centers[i]-Ys)[pos], Xs[pos], (centers[i]+Ys)[pos], Xs[pos], 
      col = gray(0.25))

  }
  axis(1, at = centers, labels = names)
  print(centers)
}
