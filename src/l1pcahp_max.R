l1pcahp <- function (X, projDim=1, center=TRUE, projPoints=FALSE, scores=TRUE, initialize="l2pca", threshold=1.0)
{
  library(cplexAPI)

  if (class(X) != "matrix") {
    if (class(X) == "data.frame")
      X <- as.matrix(X)
    else
      X <- matrix(X, ncol = 1)
  }
  if (center) {
    myMedian <- apply(X, 2, median)
    myMedMat <- matrix(rep(myMedian, nrow(X)), ncol = ncol(X), byrow=TRUE)
    X <- X-myMedMat
  }

  if (is.matrix(initialize)) {
    initV <- initialize
  } else {
    if (initialize == "l2pca") {
      mypca <- prcomp(X, center=center)
      initV <- mypca$rotation[,]
      initV <- initV[nrow(initV):1, ]
    }  else {
      if (initialize == "random") {
        initV <- matrix(runif(ncol(X)*ncol(X)), ncol=ncol(X))
      }
    }
  }
 # X <- matrix (1:12,4,3, byrow=TRUE)
  n <- ncol(X) 
  m <- nrow(X)
  k <- 1
  #Open a IBM ILOG CPLEX environment.
  env <- openEnvCPLEX()
  #Create a problem object.
  prob <- initProbCPLEX(env)
  totHyperplanes <- 0
  while (k <= n)
  {
    xx_obj <- 999999
    x_obj <- xx_obj - threshold - 1
    current_obj <- 0
    hyper_no <- 0
    point <- initV[k,]
    
    while (xx_obj - x_obj > threshold)
    {
      #cat("\nHyperplane no:", hyper_no)
      #Assign a name to the problem object.
      #chgProbNameCPLEX(env, prob, "sample")
      #Prepare data structures for the problem object. Number of columns and rows.
      
      nc <- n + m 
      nr <- 2 * m + k - 1
      #Objective function.
      obj <- c(rep(1,m),rep(0,n))
      #Right hand side.
      rhs <- c(rep(0, (2 * m)), rep(0, (k -1)))
      #Sense of the right hand side.
      sense <- c(rep("G", 2* m), rep("E", k))
      #Variable lower bounds.
      lb <-  c(rep(0, m), rep(-CPX_INFBOUND, n))
      #Variable upper bounds.
      ub <- rep(CPX_INFBOUND, nc)
      #The constraint matrix is passed in column major order format. Be careful here: all
      #indices start with 0! Begin indices of rows.
      beg <- c(seq(0,2*(m-1),2),seq(2*m, nr * (n-1) + 2*m,nr))
      #Number of non-zero elements per row.
      cnt <- c(rep(2, m),rep(nr,n))
      #Column indices.
      ind <- c( c(matrix(c(1:m-1,m:(m+m-1)), nrow=2, byrow=TRUE)) , rep(0:(nr-1), n))
      if (k > 1)
      {
        valmat <- rbind(X,-X,M)
      } else {
        valmat <- rbind(X,-X)
      }
      #Non-zero elements.
      val <- c(rep(1, (2*m)), as.vector(valmat))
      #Load problem data.
      copyLpwNamesCPLEX(env, prob, nc, nr, CPX_MIN, obj, rhs, sense, beg, cnt, ind, val, lb, ub, NULL)
      #Solve the problem using the simplex algorithm.
      status <- lpoptCPLEX(env, prob)
      if (!status)
      {
        sol <- solutionCPLEX(env, prob)
        
        current_obj <- sol$objval
        #cat("\nCurrent obj:")
        #sprintf("%.3f",current_obj) 
        
        if (current_obj > x_obj) {
          #cat("Error, the iteration is not an improvement")
          x_obj <- current_obj
        }
        xx_obj <- x_obj
        x_obj <- current_obj
        solPoint <- sol$x[(m+1):nc]
        norm_vec <- function(x) sqrt(sum(x^2))
        temp <- norm_vec(solPoint)
        point <- solPoint/temp
        #cat("\nPoint:", point)
        
        hyper_no <- hyper_no +1
      }
    }
    #cat("\nHyper number : ",hyper_no)
    totHyperplanes <- hyper_no + totHyperplanes
    
    if (k > 1)
    {
      M <- rbind(M,point)
    } else {
      M <- point
    }
    k <- k +1
  }
  
  delProbCPLEX(env, prob)
  #Close IBM ILOG CPLEX environment.
  closeEnvCPLEX(env)
  
  solution <- new.env()
  solution$loadings <-  M[nrow(M):1, ]

  solution$loadings <- t(solution$loadings[1:projDim, ])
  solution$planes = totHyperplanes
  
  if (projDim == 1)
    solution$loadings <- t(solution$loadings)
  
  
  if (scores) {
    solution$scores <- as.matrix(X %*% solution$loadings)
    totalDisp         <- sum(abs(X))
    scoreDisp         <- (apply(abs(solution$scores),2,sum))
    solution$dispExp <- scoreDisp/totalDisp
  }
  
  if (projPoints) {
    solution$projPoints <- as.matrix(t((solution$loadings) %*% t(solution$loadings) %*% t(X)))
  }
  
  solution <- as.list(solution)
  class(solution) <- "pcal1hp"
  solution
  
}  
