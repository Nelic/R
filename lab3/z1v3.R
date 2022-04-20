F = function(x)
{
	return (exp(-x)-log(abs(x)))
}

DF = function(x)
{
	return ((-1/exp(-x))-(1/x))
}

DF2 = function(x)
{
	return ((1/exp(x))+(1/x*x))
}

zbig = function(f, df2, x)
{
	kx = f(x)*df2(x)
	return (kx)
}

MNewton = function(f, df, x0, eps)
{
	x = x0
	while (abs(f(x)) > eps)
	{
		x = x-f(x)/df(x)
	}
	return (x)
}
eps = 10^(-6)

x01 = 1.3
kx01 = zbig(F, DF2, x01)
kx01

x1 = MNewton(F, DF, x01, eps)
x1