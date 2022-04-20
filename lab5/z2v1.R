Fi = function(x)
{
	z = c(1:3)
	z[1] = 1
	z[2] = x
	z[3] = x^2
	return( z )
}

MNKSolve = function (x, y, n, Fi, k)
{
	FiX = matrix(nrow=n, ncol=k)
	for(i in 1:n)
	{
		FiX[i,] = Fi(x[i])
	}
	C = matrix(nrow=k, ncol=k)
		for(j in 1:k)
		{
			for(m in 1:k)
			{
				S = 0
					for(i in 1:n)
						S = S + FiX[i,j]*FiX[i,m]
				C[j,m] = S
			}
		}
	d = c(1:k)
	for(j in 1:k)
	{
		S = 0
		for(i in 1:n)
		{
			S = S + y[i]*FiX[i,j]
		}
		d[j] = S
	}
	a = solve(C, d)
	return(a)
}

FunModel = function(x, a)
{
    return( a[1] + a[2] * x + a[3] * x ^ 2)
}

x = c(-2, -1, 0, 1, 2, 3, 4)
y = c(-0.3, 0.5, 1.5, 0.5, 0.3, -0.2, -1.2)
n = 7
k = 3

#x = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)
#y = c(9.8, 6.4, 5.2, 1.7, 2.9, 5.6, 7.2, 16.5, 27.5)
#n = 9 
#k = 3

a1 = MNKSolve(x,y,n,Fi,k)
a1

ym = FunModel(x,a1)
r = y - ym
a = -7; b = 7; n = 100
h = (b-a)/(n-1)
x1 = c(1:n)
for(i in 1:n) { x1[i] = a + h*(i-1) }
y1m = FunModel(x1,a1)
par(mfrow=c(1, 2))
plot(x, y, type="p", col="red")
lines(x1, y1m, col="blue")
plot(x, r, type="p")
lines(x, r, col="blue")
abline(h=0)
abline(v=0)
