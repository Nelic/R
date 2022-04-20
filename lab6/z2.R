F = function(x)
{
	return (x * sin(x))
}
X = 2.5
h = 0.001
tf = -1

PohidnaAnalyt = function(f, x, h, tf)
{
	if (tf == 1)
		df = (f(x+h)-f(x))/h
	else
	{
		if (tf==-1)
			df=(f(x)-f(x-h))/h
		else
		{
			if(tf==0)
				df=(f(x+h)-f(x-h))/(2*h)
		}
	}
	return(df)
}

DF = PohidnaAnalyt(F, X, h, tf)
DF