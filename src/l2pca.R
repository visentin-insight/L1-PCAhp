l2pca <- function (X, projDim=1, center=TRUE, projPoints=FALSE, scores=TRUE)
{

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
  solution <- new.env()
  
  mypca <- prcomp(X, center=center)
  
  solution$loadings <- mypca$rotation[,]

  cat(projDim,"\n")
  solution$loadings <- t(solution$loadings[1:projDim, ])
  
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
  class(solution) <- "l2pca"
  solution
  
}  
