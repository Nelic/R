Fx = function(x)
{
	f = c(1:2)
	f[1] = sin(x[1]+1)-x[2]-1
  	f[2] = 2*x[1]+cos(x[2])-2
	return(f)
}

FFx = function (x)
{
	ff = matrix(nrow=2, ncol=2)
	ff[1,1] = cos(x[1]+1)
  	ff[1,2] = -1
  	ff[2,1] = 2
  	ff[2,2] = -sin(x[2])
	return(ff)
}

x0 = c(0.65, 0.35)
N = 2

kmax = 100
eps = 0.00001

Newton = function(Fx, ffx, x, n, eps, kmax)
{
 	y = Fx(x)
	k = 0
	while ((sqrt(t(y)%*%y)) > eps)
	{
		yy = ffx(x)
		x = x - solve(yy)%*%y
		y = Fx(x)
			if (k == kmax)
				break
		k = k+1
	}
	x[n+1] = k
	return(x)
}

X = Newton(Fx, FFx, x0, N, eps, kmax)
X
Fx(X)