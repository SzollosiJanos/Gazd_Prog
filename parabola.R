rm(list=ls(all=TRUE))


x <- c(1,2,3,4,5)
y <- c(7,5,3,5,7)

summ <- function(temp){
	s=0
	for (i in 1:length(temp)) {
 		 s=s+temp[i]
	}
	return (s)
}

det <- function(a,b,c,d) {
	return (a*d-c*b)
}

det2 <- function(a,b,c,d,e,f,g,h,i){
	return ((a*det(e,f,h,i)-b*det(d,f,g,i)+c*det(d,e,g,h)))
}

oszto = det2(summ(x^4),summ(x^3),summ(x^2),summ(x^3),summ(x^2),summ(x),summ(x^2),summ(x),length(x))

a2=    det2(summ(x^2*y),summ(x^3),summ(x^2),summ(x*y),summ(x^2),summ(x),summ(y),summ(x),length(x))/oszto 
a1=    det2(summ(x^4),summ(x^2*y),summ(x^2),summ(x^3),summ(x*y),summ(x),summ(x^2),summ(y),length(x))/oszto 
a0=    det2(summ(x^4),summ(x^3),summ(x^2*y),summ(x^3),summ(x^2),summ(x*y),summ(x^2),summ(x),length(y))/oszto 


cat(sprintf("              a2= %10.8f\n",a2))
cat(sprintf("              a1= %10.8f\n",a1))
cat(sprintf("              a0= %10.8f\n",a0))