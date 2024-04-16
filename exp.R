summ <- function(X) {
    sum = 0
    for (i in 1:length(X)) {
        sum = sum + X[i]
    }
    return(sum)
}

expp <- function() {
    oszto <- ((sumXSquared * n) - (sumX * (-sumX)))
    a <- ((sumXY * n) - (sumY * (-sumX)))/oszto
    b <- ((sumX * sumY) - (sumX * sumXY))/oszto
    plot(x, y)
    curve(exp(a * x) * exp(b))
}

x <- c(1, 2, 3, 4)
y <- c(1, log(2) * exp(20 * 3), 54, 78)
n <- length(x)
y <- log(y)
sumX <- summ(x)
sumY <- summ(y)
sumXY <- summ(x*y)
sumXSquared <- summ(x*x)


expp()
