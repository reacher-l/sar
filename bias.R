#require(gtools) 
#require(purrr) 
#require(gtools)

set.seed(1234567890)

N <- c(3, 5, seq(10,100,by=10)) 
BiasMSE <- matrix(nrow=12, ncol=5)
start_time <- Sys.time()
i <- 0

est.median.bootstrap <- function(z, R) {
  t <- median(z) / log(2)
  sample_size <- length(z) 
  pwr <- sample_size^sample_size
  if(pwr < R) { 
    m.Bootstrap <- permutations(sample_size, sample_size, z, 
                              set=TRUE, repeats.allowed=TRUE) 
    return(2*t - mean(unlist(lapply(m.Bootstrap, median)))) 
  }
  else { 
    v.Bootstrap <- rep(0, R) 
    for(b in 1:R) { 
      x <- sample(z, replace = TRUE) 
      v.Bootstrap[b] <- median(x) / log(2) 
    } 
  }
  return(2*t - mean(v.Bootstrap))
  }


for(n in N){ 
  #browser()
  print(n)
  i <- i+1
  r <- ceiling(2*10^5/n) 
  v.mu1 <- array(rep(0, r)) 
  v.mu2 <- array(rep(0, r)) 
  #v.mu3 <- array(rep(0, r))
  for(j in 1:r){ 
    print(j)
    z <- rexp(n) # sample of size n from Exp(1)
    v.mu1[j] <- mean(z) 
    v.mu2[j] <- median(z) / log(2) 
    #v.mu3[j] <- est.median.bootstrap(z, 300) 
  }
  print(n)
  bias1 <- mean(v.mu1) - 1 
  eqm1 <- mean((v.mu1 - 1)^2)

  bias2 <- mean(v.mu2) - 1 
  eqm2 <- mean((v.mu2 - 1)^2)
  
  #BiasMSE[i,0] <- N[i]
  #BiasMSE[i,1] <- bias1
  #BiasMSE[i,2] <- eqm1
  #BiasMSE[i,3] <- bias2
  #BiasMSE[i,4] <- eqm2
  BiasMSE[i, ] <- c(N[i], bias1, eqm1, bias2, eqm2) 
  print(n)
  #i <- i+1
}

end_time <- Sys.time() 
end_time - start_time

inputdata <- data.frame(bias=c(BiasMSE[,3]),sample_size=c(BiasMSE[,1]))
ggplot(inputdata, aes(x=sample_size,y=bias)) +
  geom_line() + geom_point(size=4, shape=20)

