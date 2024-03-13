rm(list=ls(all=TRUE))

x <- c(1,3,4,5,10)
y <- c(3,5,7,9,11)

summ <- function(temp){
	s=0
	for (i in 1:length(x)) {
 		 s=s+temp[i]
	}
	return (s)
}

n <- length(x)
x_sum <- summ(x)
y_sum <- summ(y)


det <- function(a,b,c,d) {
	return (a*d-c*b)
}





a <- det(summ(x*y),y_sum,x_sum,n)/det(summ(x^2),x_sum,x_sum,n)

b <- (y_sum-a*x_sum)/n

cat(sprintf("              y %10.8f\n",b))
cat(sprintf("              x %10.8f\n",a))

line <- a*x+b
cat(sprintf("              y %10.8f\n",line ))

plot(x,y,col=2,pch=19,cex=2,xlim = c(0, 10),ylim = c(0, 12))
lines(x,line,col=3,lwd=5)