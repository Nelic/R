MGaussJord = function(A,n)
{
	E = matrix(0, nrow = n, ncol = n)
	for(i in 1:n)
		E[i,i] = 1
	for(k in 1:n)
	{
		for(j in 1:n)
			E[k,j] = E[k,j]/A[k,k]
		if(k==n)
			break
		for(j in (k+1):n)
			A[k,j] = A[k,j]/A[k,k]
		for(i in (k+1):n)
		{
			for(j in (k+1):n)
				A[i,j] = A[i,j] - A[i,k]*A[k,j]
			for(j in 1:n)
				E[i,j] = E[i,j] - A[i,k]*E[k,j]
		}
	}
	for(k in (n-1):1)
	{
		for(i in (k+1):n)
			for(j in 1:n)
				E[k,j] = E[k,j] - A[k,i]*E[i,j]
	}
	return(E)
}

A = cbind(c(1.14, 0.42, -0.71), c(-2.15, -1.13, 0.81), c(-5.11, 7.05, -0.02))
n = 3

A1 = MGaussJord(A,n)
A1

E1 = A%*%A1
E1

A2 = solve(A)
A2
