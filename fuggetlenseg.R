summ <- function(temp){
	s=0
	for (i in 1:length(temp)) {
 		 s=s+temp[i]
	}
	return (s)
}

colsum <- function(m){
	s=c()
	for(i in 1:ncol(m)){
		temp=0
		for(l in 1:nrow(m)){
			temp=temp+m[l,i]
		}
		s=append(s,temp)
	}
	return(s)
}

rowsum <- function(m){
	s=c()
	for(i in 1:nrow(m)){
		temp=0
		for(l in 1:ncol(m)){
			temp=temp+m[i,l]
		}
		s=append(s,temp)
	}
	return(s)
}

fuggetlenseg <- function(K){
	Frow = rowsum(K)
	Fcol = colsum(K)
	n = summ(Frow)
	r = nrow(K)
	s = ncol(K)
	sumS = 0;
	for (i in 1:r){
		for (j in 1:s){
			o = (K[i,j] - (Frow[i]*Fcol[j])/n)^2
			sumS = sumS + (o/(Frow[i]*Fcol[j]))
		}
	}	
	sumS = n*sumS;
	chi=qchisq(p=.90, df=(r-1)*(s-1))


	cat('result: ',sumS ,' chi: ',chi,'\n');
	if (sumS <chi) cat('Az erteket elfogadjuk!\n')
	else cat('Az erteket nem fogadjuk el!\n')
}

fuggetlenseg (matrix(c(42,17,28,89,3,21),nrow=2,ncol=3))