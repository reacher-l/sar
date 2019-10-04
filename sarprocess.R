
dKI <- function(z, p_alpha, p_lambda, p_Looks, log=FALSE) {
  
  if(log==FALSE) {
    
    lLz <- p_lambda * p_Looks* z
    
    return((2*p_lambda*p_Looks/(gamma(p_alpha)*gamma(p_Looks))) *
             (lLz)^((p_alpha+p_Looks)/2-1) *
             besselK(x = 2*sqrt(lLz), nu = p_alpha-p_Looks)
    )
  }
  
}
k <-function(z,a,r,L){
  L**L*gamma(L-a)*z**(L-1)/(r**a*gamma(L)*gamma(-a)*(r+L*z)**(L-a))
}


r <- 0.299
g <- 0.587
b <- 0.114
pic <- readJPEG("C:/Users/Administrator/Desktop/sar/cropped.jpg")
R <- pic[,,1]
G <- pic[,,2]
B <- pic[,,3]


gray_hist <- r*R + g*G + b*B  
a <- hist(gray_hist, 
          breaks = seq(0,1,0.02), 
          freq = F)
b <- matrix(unlist(a))
b <- b[103:152]
b <- rev(b)
inputdata <- data.frame(gray_value=seq(0,0.998,0.02),gray_frequency=b)
ggplot(data=inputdata,aes(x=gray_value,y=as.numeric(gray_frequency)/16)) + 
  geom_bar(stat="identity") +
  stat_function(fun=dKI, geom = "line", size=2, col="black", args = list(p_alpha=5, p_lambda=16.5, p_Looks=60)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="red", args = list(shape=3, scale=1/10)) +
  stat_function(fun=k, geom = "line", size=2, col="blue", args = list(a=-5.3,r=1.7,L=50)) +
  xlab("Grey Value") + ylab("Gray Histogram and Gamma Densities")

