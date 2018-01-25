
### Long Number Multiplication
# x and y are numeric vectors. Each element of these vectors has 
# an integer value in the range 0 to 999.Each of these vectors stores 
# one number. The first element of x and y contain the most significant digits.
# Examples:
# To store 123456789 into x we use the R command x = c(123,456,789)
# To store 45000892012 into y we use the R command y = c(987,654,321) 
# z = long.mult(x,y) 
# z 
# [1] 121 932 631 112 635 269

long.mult <- function(x, y ){
  ## stop if x or y contains non-numeric elements
  if( !is.numeric(x) || !is.numeric(y)) {   
    stop("x and y must be numeric vectors!")
  }
  
  ## stop if both x and y are empty
  if(max(abs(floor(x) - x)) > 0 || max(abs(floor(y) - y))) { 
   stop("Both x and y must only have 0 digit!") 
  }
  
  ## stop if any element is not from 0 to 999
  if( any(x < 0)  || any(x > 999) || any(y < 0) || any(y > 999) ) {
    stop("x and y must have values within [0, 999]!") 
  }
  
  # if any of x and y are zero, result is zero
  if( all(x == 0) || all(y == 0)){
   return(0)
  }
  
  ## reverse input vectors
  x.reverse = rev(x)
  y.reverse = rev(y)
  
  n1= length(x) 
  n2= length(y) 
  
  # initialize the result vector z with length is the sum of length of input vectors
  z = numeric(n1 + n2)

  ## itetare through x
  for( i in 1 : n1){
    z.index = i ## z.index will be assign to the value of i here
    ## iterate through y
    for( j in 1 :n2){
      z[z.index] = x.reverse[i] * y.reverse[j] + z[z.index]
      z.index = z.index + 1
    }
  }

  carry = 0 #initiate the carryoevr 
  for(i in 1 : (length(z) - 1)) {
    carry = as.integer( z[i] / 1000 ) ## convert a double var to int var and assign to carryover
    z[i] = z[i] - carry * 1000        ## calculate the actual value of current element
    z[i + 1] = z[i + 1] + carry     
    i = i + 1
  }
  
  ## reverse z 
  z= rev(z)
  
  ## Corner case when leading zeros appears
  if(z[1] == 0) {
    leading.zero.counter = 1 ## initiate
    while (z[leading.zero.counter] == 0) {
      leading.zero.counter = leading.zero.counter + 1
    }
    return(z[-1: (1- leading.zero.counter)]) ## remove numbers of continious leading zeors
                                                     ## and return
  }
  
  ## return z if none leading zeros
  return(z) 
}

