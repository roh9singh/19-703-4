# This function adds N observations to the Variables L1 and L2
AddInB <- function (L1,L2,N,pval){
  # Generate normal RV with mean = 0, var = 1 and appends it to the variables L1 and L2
  L1 <- append(L1, rnorm(N, 0, 1)) 
  L2 <- append(L2, rnorm(N, 0, 1))
  
  #Two-Sample t-test
  t.test1 <- t.test(L1,L2, var.equal = TRUE) 
  
  # checks if the t-test is significant and stores 1, otherwise 0
  newsignif <- ifelse(t.test1$p.value < pval,1, 0)
  
  return(newsignif) #value returned by the function
}


#This function mimics Situation B
SituationB <- function(pval){
  
  L1 <- rnorm(20, 0, 1)# Generate normal RV with mean = 0, var = 1
  L2 <- rnorm(20, 0, 1)# Generate normal RV with mean = 0, var = 1
  
  #Two-Sample t-test
  t.test1 <- t.test(L1, L2, var.equal = TRUE) 
  
  # checks if the t-test is significant and stores 1, otherwise 0
  signif <- ifelse(t.test1$p.value < pval,1, 0)
  
  # Checks if the signif variable is 0 and calls the AddInB function to add  
  # 10 more observation, otherwise retains the same value
  signif <- ifelse(signif == 0,AddInB(L1,L2,10,pval),signif)
  
  return(signif) #value returned by the function
}

replicates <- replicate(15000, SituationB(.05))
SitBSim <- mean(replicates)
print(SitBSim)
