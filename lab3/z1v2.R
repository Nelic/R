MHord = function(f, a, b, eps)
{
	x = a+abs(f(a)/(f(b)-f(a)))*(b-a)
	while (abs(f(x)) > eps)
	{
	x = a+abs(f(a)/(f(b)-f(a)))*(b-a)
	if (f(a)*f(x) < 0) b = x
	else a = x
	}
	return (x)
}

eps = 10^(-6)
a1 = 1
b1 = 2

s1 = MHord(F, a1, b1, eps)
s1
