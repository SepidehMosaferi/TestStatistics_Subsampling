#-------------------------------------------------------------------------------
# Title: R code for the Selection of Optimal Bandwidths and Related Plots 
#        in the Application
# Author: Sepideh Mosaferi
# Date: July 2022
#-------------------------------------------------------------------------------
require("PLRModels")

# read data
glinear_Spain <- read.table("/Users/sepidehmosaferi/Desktop/glinear_Spain.txt", header = TRUE)
gquadratic_France <- read.table("/Users/sepidehmosaferi/Desktop/gquadratic_France.txt", header = TRUE)


## Spain
# cross-validation
data <- cbind(glinear_Spain[,2],glinear_Spain[,1])

CVprocess <- np.cv(data = data, h.seq = NULL, num.h = 50, w = NULL, num.ln = 1, 
                   ln.0 = 1, step.ln = 1, estimator = "NW", kernel = "gaussian")

CVprocess$h.opt[2,1]; CVprocess$CV.opt

# Nadaraya-Watson regression function estimator
nw <- function(x, X, Y, h, K = dnorm) {
  
  # arguments
  # x: evaluation points
  # X: vector (size n) with the predictors
  # Y: vector (size n) with the response variable
  # h: bandwidth
  # K: kernel
  
  # matrix of size n x length(x) (rbind() is called for ensuring a matrix
  # output if x is a scalar)
  Kx <- rbind(sapply(X, function(Xi) K((x - Xi) / h) / h))
  
  # weights
  W <- Kx / rowSums(Kx) 
  
  # means at x ("drop" to drop the matrix attributes)
  drop(W %*% Y)
  
}

par(mfrow=c(1,2))
par(mar=c(5,5,5,5)+0.2)

plot(CVprocess$h.seq,CVprocess$CV,ylab="Objective",xlab="Bandwidth (h)",
     main="Spain",cex=1.5,font.main=2,lwd=2,cex.lab=1.5, cex.axis=1.5)
abline(v=CVprocess$h.opt$matrix.0..2..num.ln.[2],col = 2, lwd = 2)

x_grid <- seq(-10, 20, l = 500)
plot(data[,2], data[,1],xlab="log(GDP)",ylab="log(CO2)",main="Spain",
     cex=1.5,font.main=2,lwd=2,cex.lab=1.5, cex.axis=1.5)
lines(x_grid, nw(x = x_grid, X = data[,2], Y = data[,1], 
                 h = CVprocess$h.opt$matrix.0..2..num.ln.[2]), 
      col = "black", lwd=2, lty=4)
lines(glinear_Spain$x_Spain,glinear_Spain$g_linear,lwd=2,col="lightblue",lty=1)
legend(8.1, 5.75, legend=c("N-W", expression(H[A])), col = c("black","lightblue"),lty = c(4,1))


#----------------------------------------------------------
## France
# cross-validation
data <- cbind(gquadratic_France[,2],gquadratic_France[,1])

CVprocess <- np.cv(data = data, h.seq = NULL, num.h = 50, w = NULL, num.ln = 1, 
                   ln.0 = 1, step.ln = 1, estimator = "NW", kernel = "gaussian")

CVprocess$h.opt[2,1]; CVprocess$CV.opt

par(mfrow=c(1,2))
par(mar=c(5,5,5,5)+0.2)

plot(CVprocess$h.seq,CVprocess$CV,ylab="Objective",xlab="Bandwidth (h)",
     main="France",cex=1.5,font.main=2,lwd=2,cex.lab=1.5, cex.axis=1.5)
abline(v=CVprocess$h.opt$matrix.0..2..num.ln.[2],col = 2, lwd = 2)

x_grid <- seq(-10, 20, l = 500)
plot(data[,2], data[,1],xlab="log(GDP)",ylab="log(CO2)",main="France",
     cex=1.5,font.main=2,lwd=2,cex.lab=1.5, cex.axis=1.5)
lines(x_grid, nw(x = x_grid, X = data[,2], Y = data[,1], 
                 h = CVprocess$h.opt$matrix.0..2..num.ln.[2]), 
      col = "black", lwd=2, lty=4)
lines(gquadratic_France$x_France,gquadratic_France$g_quadratic,lwd=2,col="lightblue",lty=1)
legend(8.99, 5.9, legend=c("N-W", expression(H[B])), col = c("black","lightblue"),lty = c(4,1))

