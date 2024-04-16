x <- c(10, 42, 64, 23, 112, 21)
y <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)  
z <- 200


illeszkedes <- function(k, p, N){
	khi = 0
	n = length(k)

	for (i in 1:n) {
        khi = khi + ((( k[i] - N * p[i] ) ^2 ) / ( N * p[i] ))
    	}
    	return(khi)
}

illeszkedes(x, y, z)
