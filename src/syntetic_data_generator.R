.libPaths( c( .libPaths(), "~/R/x86_64-redhat-linux-gnu-library/3.0/") )
dataGen <- function (obs = 1000, m = 10, q = 2, p = 1, mu = 25, ntype = 1 ,percoutliers = 0.1)
{
  library(ExtDist)
    if (ntype == 1)
    {
      ## Laplacian noise
      clean <- matrix(rLaplace(q * obs , mu = 0, b = 10),ncol = q)
      if (p > 0)
      {
        poutliers <- matrix(rLaplace(p * obs * percoutliers , mu = mu, b = 0.01),ncol = p)
        poutliers <- cbind(poutliers,matrix(rLaplace((m-p-q) * obs* percoutliers , mu = 0, b = 1),ncol = (m-p-q)))
        noise <- rbind(matrix(rLaplace((m-q) * (obs* (1-percoutliers)) , mu = 0, b = 1),ncol = (m-q)),poutliers)
        
      } else {
        noise <- matrix(rLaplace((m-q) * obs , mu = 0, b = 1),ncol = (m-q))
      }
      dataset <- cbind(clean,noise)
    } else {
      ## Gaussian noise
      clean <- matrix(rnorm(q * obs , mean = 0, sd = 10),ncol = q)
      if (p > 0)
      {
        poutliers <- matrix(rnorm(p * obs * percoutliers , mean = mu, sd = 0.01),ncol = p)
        poutliers <- cbind(poutliers,matrix(rnorm((m-p-q) * obs* percoutliers , mean = 0, sd = 1),ncol = (m-p-q)))
        noise <- rbind(matrix(rnorm((m-q) * (obs* (1-percoutliers)) , mean = 0, sd = 1),ncol = (m-q)),poutliers)
        
      } else {
        noise <- matrix(rnorm((m-q) * obs , mean = 0, sd = 1),ncol = (m-q))
      }
      dataset <- cbind(clean,noise)
    }
    
    dataset
  
}