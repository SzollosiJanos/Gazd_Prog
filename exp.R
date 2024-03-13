rm(list=ls(all=TRUE))

x <- c(1,2,3,4)
y <- c(2.6,7.2,20.4,55)

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
line <- exp(a)*exp(b*x)
plot(x,line,col=3,lwd=5,type="l",xlim = c(0, 5),ylim = c(0, 60))
points(x,y,col=2,pch=19,cex=2)



