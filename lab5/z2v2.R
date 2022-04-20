Fi = function(x)
{
    z = c( 1:3 )
    z[1] = 1
    z[2] = x
    z[3] = x^2
    return ( z )
}

MNKSolvePolinom = function(x, y, n, k)
{
    C = matrix(nrow = k, ncol = k)
    for (j in 1:k)
	{

      for (m in 1:k)
	{
            s = 0
            for( i in 1:n)
		{
                	s = s + x[i]^(m + j - 2)
            }
            C[j, m] = s
        	}
    	}

    	d = c(1:k)
    	for( j in 1:k)
	{
        
        s = 0
        for( i in 1:n)
		{
            	s = s + y[i] * x[i]^(j - 1)
        	}
        	d[j] = s
    	}
    	a = solve(C, d)
    	return ( a )
}

f = function(x,a)
{
    return (a[1] + a[2] * x + a[3] * x^2 + a[4] * x^3)
} 

x = c(-2, -1, 0, 1, 2, 3, 4)
y = c(-0.3, 0.5, 1.5, 0.5, 0.3, -0.2, -1.2)
n = 7
k = 4
    
#x = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)
#y = c(9.8, 6.4, 5.2, 1.7, 2.9, 5.6, 7.2, 16.5, 27.5)
#n = 9
#k = 4

a1 = MNKSolvePolinom(x, y, n, k)
a1
  
a = -3; b = 6; n = 100
h = (b - a) / (n - 1)
x1 = c(1:n)
y1 = c(1:n)

for(i in 1:n) 
{
    x1[i] = a + h * (i - 1)
    y1[i] = f(x1[i], a1)
}

plot(x, y, type="p", col="red")
lines(x1, y1, col="blue")



    