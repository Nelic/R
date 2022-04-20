MGaussSLE = function(A,b,n)
{
	for(k in 1:(n-1))
	{
		for(j in (k+1):n)
			A[k,j] = A[k,j]/A[k,k]
		b[k] = b[k]/A[k,k]
		for(i in (k+1):n)
		{
			for(j in (k+1):n)
				A[i,j] = A[i,j]-A[i,k]*A[k,j]
			b[i] = b[i] - A[i,k]*b[k]
		}
	}
	b[n] = b[n]/A[n,n]
	for(k in (n-1):1)
	{
		for(j in (k+1):n)
			b[k] = b[k]-A[k,j]*b[j]
	}
	return (b)
}

A = cbind(c(1.14, 0.42, -0.71), c(-2.15, -1.13, 0.81), c(-5.11, 7.05, -0.02))
b = c(2.05, 0.80, -1.07)
n = 3
A
b
n

x = MGaussSLE(A, b, n)
x