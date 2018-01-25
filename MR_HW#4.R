
##############################################################################
#Assignment #4 
# The detect.misclass function is used to detect and correct misclassifications.
# The function includes two arguments: class.v and p. 

# class.v is a vector containing the classification for the observations.Here, the elements 
  ##in class.v only includes 1 or 2. The two clusters are well-separated.
# p is a matrix that stores measured attributes for each observation.
  ## Length(class.v) is equal to nrow(p). The number of attributes measured for each 
  ## observation is equal to ncol(p). 
  ### For example, if p is a mxn matrix, we have m observations and n attributes.

# The output of the function is a list containing three items.
  ## The first item is "err.found". It is set to TRUE if a misclassification was detected. Otherwise it is set to FALSE. 
  ## The second item is "err.loc". It is a vector containing the indices of the misclassified observations. 
  ## The last item is "new.class". It is a vector that contains the corrected classification for an observation.
  ### When the "err.found" item is FALSE, the "err.loc" item and the "new.class" item are both set to NULL.

  ### For example, suppose we detect that observation 5 should have been classified as a 1, and observation 569 should 
  ### have been classified as a 2.
  ### The output is:
  ### $err.found
  ### [1] TRUE
  ### $err.loc
  ### [1] 5 569
  ### $new.class
  ### [1] 1 2
##############################################################################

detect.misclass <- function(class.v,p){ 
  # Condition check
  ## make sure that length of class.v is equal to number of rows in p
  if( length(class.v) != nrow(p)){
    stop("Elements in class.v must be equal to number of rows in p")
  }
  
  ## check class.v only contains 1 or 2 
  for (index in (1:length(class.v))){
    if (class.v[index]!=1 && class.v[index]!=2){
      stop("class.v can only include 1 or 2")
    }
  }
  
  # initiate err.found to be false if there's no error found
  err.found = FALSE
  
  # prepare vectors that we need later
  err.loc = numeric()
  p.dis=numeric()
  p.temp=numeric()
  new.class = numeric()
  
  # set n equals the number of rows in p
  n=nrow(p)
  
  # initiate the index in err.loc begin from 1
  err.loc.index=1
  
  # define k as 5
  k=5

  # begin the outter loop
  # loop for each observation in p
  for (i in (1:n)){
   # prepare a p.temp matrix repeat n times of one observation
   p.temp = matrix(rep(p[i,],n),nrow=n,byrow = TRUE) 
   # calculate the distance between one observation with all observations
   p.distance=rowSums((p - p.temp)^2)
   # order() function is used to store the original location in p.distance for each element in the ascending sorted vector
   p.loc = order(p.distance)
   
   # compare class of one observation with that of k nearest observations (k smallest distance)
   # initiate different counts as 0, and same counts as -1 to offset its own effect (since the distance between one observation
   ## and its own is always zero, so that provides no information for whether it is misclassified)
   diff=0
   same=-1
   
   # begin of the inner loop
   # loop for k+1 observations to make sure we have k nearest observations to compare
   for (j in (1:(k+1))){
     # when the class of one observation is different from another observation, diff adds by one
     if (class.v[i] != class.v[p.loc[j]]){
       diff = diff + 1
     }
     # when the class of one observation is the same with another observation, same adds by one
     else{
       same = same + 1
      }
   }
   
   # compare the one observation with the k nearest observations, if the same count is smaller than diff count, it is misclassified
   if (same < diff){
     err.found = TRUE
     # store the location of the misclassified observation into err.loc
     err.loc[err.loc.index] = i
     # correct the class of the misclassified observation
     correct = 2 %/% class.v[i]
     # store the correct class into new.class 
     new.class[err.loc.index] = correct
     # update the error location index for next round of outter loop
     err.loc.index = err.loc.index + 1
   }
   
   # if no observation has same count smaller than diff count, then err.found remains false, and other two vectors
   # equal NULL
   if(err.found == FALSE)
   { err.found = FALSE
   err.loc = NULL
   new.class = NULL
   }
  }
  
  # return a list of err.found, err.loc and new.class
  List = list("err.found"=err.found,"err.loc"=err.loc,"new.class"=new.class)
  return(List)   
}


