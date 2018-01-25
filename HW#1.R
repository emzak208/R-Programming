## HW 1 ##
######## Q1 ###########
merge.sort <-function (in1,in2)
{
result <- c() 
   while (min(length(in1),length(in2))>0) # while some are left in both vectors
   { # put the smallest to the first 
      if (in1[1] < in2[1])
      { # Add value to result then remove the value from v1 
       result = c(result, in1[1])
       in1 = in1[-1] 
      }
      else
      { # Add value to result then remove the value from v2
       result = c(result, in2[1])
       in2 = in2[-1] 
      }
    }
  if ( length(in1) > 0 ) 
    result = c(result,in1)
  else 
    result= c(result,in2)
return (result)
}

# formatC(merge.sort(int1,int2),format = 'G',drop0trailing = FALSE)
# the above line is to remove the trailing zero

######## Q2 ###############
bin.data <- function(x,bins)
{  # This for loop is to check is bins vactoer is increasing order 
  for (j in 2:length(bins))
  {
    if ( bins[j] <= bins[j-1])
    {
      stop('Opps')
    }
  }
  
  # the second part is to bin the vector
  # create a numeric object with length #bins +1
  out = numeric(length = length(bins) + 1) # the # of elements in out = #bins +1
  for (i in 1:length(x))
  { # iterate through x
     # reset j to start with 1
    j=1
    while ( j <= length(bins) && x[i] > bins[j])
    { # interate until the x[i] <= bins[j]
      j = j+1
    }
    {out[j] = out[j] + 1} # if the bin was hit, bin value increase by 1
  }
  return (out)
}

### alternative for Q2
# Do a binary search on the bin
bins = c(bins,Inf)

for ( i in i:n.x)
  {
    j =sum(x[i] >bins) +1
    out[j]= out[j] +1
   }
return (out)


  
  







