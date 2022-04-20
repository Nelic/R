X = c(0.15, 0.19, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70)
Y = c(0.861, 0.8274, 0.819, 0.779, 0.741, 0.705, 0.670, 0.638, 0.607, 0.577, 0.548, 0.497, 0.472)
N = 12

PohidnaTabl = function(x, y, n)
{
	df = c(1:n)
	h = x[2]-x[1]
	for (i in 1:n)
	{
		if (i==1)
			df[i]=(y[2]-y[1])/h
		else
		{
			if(i==n)
				df[i]=(y[n]-y[n-1])/h
			else df[i]=(y[i+1]-y[i-1])/(2*h)
		}
	}
	return (df)
}

DF = PohidnaTabl(X, Y, N)
DF