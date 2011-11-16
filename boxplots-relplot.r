# Original code written by Rand Wilcox, 
# http://dornsife.usc.edu/labs/rwilcox
# http://www-rcf.usc.edu/~rwilcox/
# 
# Extracted from 
# http://www-rcf.usc.edu/~rwilcox/allfun-v9

# Compute the measures of location, scale and correlation
#   used in the
#   bivariate boxplot of Goldberg and Iglewicz,
#   Technometrics, 1992, 34, 307-320.
#
#   The code in relplot plots the boxplot.
#
#   This code assumes the data are in xv and yv
#
#   This code uses the function biloc and
#   bivar
#
relplot <- function(xv, yv, C = 36, epsilon = 1e-04, 
    plotit = T, xlab = "X", ylab = "Y", xlim = range(xv), ylim = range(yv)) {
    plotit <- as.logical(plotit)
    tx <- biloc(xv)
    ty <- biloc(yv)
    sx <- sqrt(bivar(xv))
    sy <- sqrt(bivar(yv))
    z1 <- (xv - tx)/sx + (yv - ty)/sy
    z2 <- (xv - tx)/sx - (yv - ty)/sy
    ee <- ((z1 - biloc(z1))/sqrt(bivar(z1)))^2 + ((z2 - biloc(z2))/sqrt(bivar(z2)))^2
    w <- (1 - ee/C)^2
    if (length(w[w == 0]) >= length(xv)/2) 
        warning("More than half of the w values equal zero")
    sumw <- sum(w[ee < C])
    tempx <- w * xv
    txb <- sum(tempx[ee < C])/sumw
    tempy <- w * yv
    tyb <- sum(tempy[ee < C])/sumw
    tempxy <- w * (xv - txb) * (yv - tyb)
    tempx <- w * (xv - txb)^2
    tempy <- w * (yv - tyb)^2
    sxb <- sum((tempx[ee < C]))/sumw
    syb <- sum((tempy[ee < C]))/sumw
    rb <- sum(tempxy[ee < C])/(sqrt(sxb * syb) * sumw)
    z1 <- ((xv - txb)/sqrt(sxb) + (yv - tyb)/sqrt(syb))/sqrt(2 * 
        (1 + rb))
    z2 <- ((xv - txb)/sqrt(sxb) - (yv - tyb)/sqrt(syb))/sqrt(2 * 
        (1 - rb))
    wo <- w
    ee <- z1^2 + z2^2
    w <- (1 - ee/C)^2
    sumw <- sum(w[ee < C])
    tempx <- w * xv
    txb <- sum(tempx[ee < C])/sumw
    tempy <- w * yv
    tyb <- sum(tempy[ee < C])/sumw
    tempxy <- w * (xv - txb) * (yv - tyb)
    tempx <- w * (xv - txb)^2
    tempy <- w * (yv - tyb)^2
    sxb <- sum((tempx[ee < C]))/sumw
    syb <- sum((tempy[ee < C]))/sumw
    rb <- sum(tempxy[ee < C])/(sqrt(sxb * syb) * sumw)
    z1 <- ((xv - txb)/sqrt(sxb) + (yv - tyb)/sqrt(syb))/sqrt(2 * 
        (1 + rb))
    z2 <- ((xv - txb)/sqrt(sxb) - (yv - tyb)/sqrt(syb))/sqrt(2 * 
        (1 - rb))
    iter <- 0
    while (iter <= 10) {
        iter <= iter + 1
        ee <- z1^2 + z2^2
        w <- (1 - ee/C)^2
        sumw <- sum(w[ee < C])
        tempx <- w * xv
        txb <- sum(tempx[ee < C])/sumw
        tempy <- w * yv
        tyb <- sum(tempy[ee < C])/sumw
        tempxy <- w * (xv - txb) * (yv - tyb)
        tempx <- w * (xv - txb)^2
        tempy <- w * (yv - tyb)^2
        sxb <- sum((tempx[ee < C]))/sumw
        syb <- sum((tempy[ee < C]))/sumw
        rb <- sum(tempxy[ee < C])/(sqrt(sxb * syb) * sumw)
        z1 <- ((xv - txb)/sqrt(sxb) + (yv - tyb)/sqrt(syb))/sqrt(2 * 
            (1 + rb))
        z2 <- ((xv - txb)/sqrt(sxb) - (yv - tyb)/sqrt(syb))/sqrt(2 * 
            (1 - rb))
        wo <- w
        ee <- z1^2 + z2^2
        w <- (1 - ee/C)^2
        dif <- w - wo
        crit <- sum(dif^2)/(mean(w))^2
        if (crit <= epsilon) 
            break
    }
    if (plotit) {
        em <- median(sqrt(ee))
        r1 <- em * sqrt((1 + rb)/2)
        r2 <- em * sqrt((1 - rb)/2)
        temp <- c(0:179)
        thet <- 2 * 3.141593 * temp/180
        theta1 <- r1 * cos(thet)
        theta2 <- r2 * sin(thet)
        xplot1 <- txb + (theta1 + theta2) * sqrt(sxb)
        yplot1 <- tyb + (theta1 - theta2) * sqrt(syb)
        emax <- max(sqrt(ee[ee < 7 * em^2]))
        r1 <- emax * sqrt((1 + rb)/2)
        r2 <- emax * sqrt((1 - rb)/2)
        theta1 <- r1 * cos(thet)
        theta2 <- r2 * sin(thet)
        xplot <- txb + (theta1 + theta2) * sqrt(sxb)
        yplot <- tyb + (theta1 - theta2) * sqrt(syb)
        totx <- c(xv, xplot, xplot1)
        toty <- c(yv, yplot, yplot1)
        plot(totx, toty, type = "n", xlab = xlab, ylab = ylab, 
            ylim = ylim, xlim = xlim)
        polygon(xplot, yplot, col = "grey90", border = NA)
        polygon(xplot1, yplot1, col = "grey70", border = NA)
        points(xv, yv)
    }
    
    invisible(list(mest = c(txb, tyb), mvar = c(sxb, syb), mrho = rb))
}

# compute biweight measure of location
# This function is used by relplot
biloc <- function(x) {
    m <- median(x)
    u <- abs((x - m)/(9 * mad(x) * qnorm(0.75)))
    top <- sum((x[u <= 1] - m) * (1 - u[u <= 1]^2)^2)
    bot <- sum((1 - u[u <= 1]^2)^2)
    bi <- m + top/bot^2
    bi
}

# compute biweight midvariance of x
bivar <- function(x) {
    m <- median(x)
    u <- abs((x - m)/(9 * qnorm(0.75) * mad(x)))
    av <- ifelse(u < 1, 1, 0)
    top <- length(x) * sum(av * (x - m)^2 * (1 - u^2)^4)
    bot <- sum(av * (1 - u^2) * (1 - 5 * u^2))
    bi <- top/bot^2
    bi
} 
