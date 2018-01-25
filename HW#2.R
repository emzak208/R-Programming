
# stop iteration condition: |(xK+1-xk)|< tol
# Newton's method Function: x(K + 1) = (1 / N) * ((N - 1) * x(K) + A / x(K) ^ (N - 1))
# xpre : previous x value
# xk: current x value
# tol : tolerance for the difference between xk and xpre
# The smaller tol, the more accurate the result
# difx : the absolute value of the difference betwwen the current x value 
       # and the previous x value
nth.root <- function(a,n,tol)
{
   xpre = a # initialize a random number as xpre, here I just assign a 
   difx = 10 * tol # initialize the absolute difference, should be larger than tol
   xk = xpre - 3 # initialize current x value ( any number will work)
   while (difx > tol) # continue iteration until difx < tol
   { # calculate current value from previous value using Newton's Method
      xk = (1/n) * ((n-1) * xpre + a /xpre ^ (n-1)) # newton's method formula
      difx = abs(xk - xpre) 
      xpre = xk # assign current x value to previous x value, interate again 
      if (tol < 1e-15)
      { # when tol < 1e-15, stop the iteration 
        stop('Error') 
      }
    } 
   return (xk) # reach the point where difx < tol. Then return the last x value as the root
}
