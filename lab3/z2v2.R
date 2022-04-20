N = 2
Gx = function(x)
{
	g = c(1:2)
	g[1] = (2-cos(x[2]))/2
  	g[2] = sin(x[1]+1)-1
	return(g)
}

Fx = function(x)
{
	f = c(1:2)
	f[1] = sin(x[1]+1)-x[2]-1
	f[2] = 2*x[1]+cos(x[2])-2
	return(f)
}

x0 = c(0.65, 0.35)
kmax = 100
eps = 0.00001

MetIter = function(G,x,n,eps,kmax)
{
	k = 0
	repeat
	{
		xk = G(x)
		y = xk - G(xk)
		if (((sqrt(t(y)%*%y))<= eps) | (k == kmax))
		{
			break
		}
		x = xk
		k = k + 1
	}
	x[n+1] = k
	return(x)
}

X = MetIter(Gx,x0,N,eps,kmax)
X

Fx(X)

