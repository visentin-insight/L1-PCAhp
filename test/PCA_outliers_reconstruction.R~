pcaOutliers <- function(ds, maxfeature, minfeature = 1, outliers, filename = FALSE, center = "mean", scale = "svd")
{
  #import functions
  source("~/l1pcahp/src/class_data.R") #dataset manager
  source("~/l1pcahp/src/l1pcahp_max.R")    #l1pcahp function
  source("~/l1pcahp/src/l2pca.R")    #10cvknn algorithm
  library("pcaL1")
  library("pcaPP")
  library("MASS")
  #pcaOutliers("milk",3,1,c(70,1,2,41))
  
  
  projection <- function(loadings, data) {
    result <- as.matrix(t((loadings) %*% t(loadings) %*% t(data)))
  }
  
  result_matrixR <- rep(0, 10)
  result_matrixV<- rep(0, 10)
  
  #shuffle the dataset
  
  exp_data <- classDataset(ds)
  data_set <- exp_data$attributes
  
  if (center == "mean")
    data_set <- scale(data_set, center = TRUE, scale = FALSE)
  if (center == "median")
    data_set <- scale(data_set,center= apply(data_set, 2, FUN = median), scale = FALSE)
  
  if (scale == "svd")
    data_set <- scale(data_set, center = FALSE, scale = TRUE)
  if (scale == "MAD")
    data_set <- scale(data_set,center= FALSE, scale =apply(data_set,2, mad))
  
  
  
  for (projDim in minfeature:maxfeature){
    ##### PCA #####
    data_l2pca <- l2pca(data_set,projDim = projDim,center = TRUE, scores = TRUE, projPoints = FALSE)
    data_PCAproj <- PCAproj(data_set,projDim)
    data_PCAgrid <- PCAgrid(data_set,projDim)
    data_pcal1 <- pcal1(data_set,projDim = projDim,center = TRUE, scores = TRUE, projPoints = FALSE, initialize = "l2pca")
    data_l1pca <- l1pca(data_set,projDim = projDim,center = TRUE, projPoints = FALSE, initialize = "l2pca", tolerance = 0.0001, iterations = 10)
    data_l1pcastar <- l1pcastar(data_set,projDim = projDim,center = TRUE, scores = TRUE, projPoints = FALSE)
    data_l1plcahp <- l1pcahp(data_set,projDim = projDim, center = TRUE, scores = TRUE,  initialize = "l2pca", threshold = 0.001)
    starscore <- as.matrix(data_set %*% data_l1pcastar$loadings[,1:projDim])
    data_r <- list(projection(data_l2pca$loadings,data_set), projection(data_PCAproj$loadings,data_set), projection(data_PCAgrid$loadings,data_set), projection(data_pcal1$loadings,data_set), projection(data_l1pca$loadings,data_set), projection(data_l1pcastar$loadings[,1:projDim],data_set), projection(data_l1plcahp$loadings,data_set))
    data_v <- list(data_l2pca$scores, data_PCAproj$scores, data_PCAgrid$scores,data_pcal1$scores,data_l1pca$scores,starscore,data_l1plcahp$scores)
    
        # apply the classificator to the dataset reduced
    resultR <- sapply (data_r, function(x,y,outliers) sum(abs(x[-outliers,]-y[-outliers,])), y = data_set, outliers = outliers) 
    result_matrixR <- rbind(result_matrixR, c(resultR, exp_data$id,projDim,1))
    if (projDim == 1)
    {
      resultV <- sapply (data_v, function(x,outliers) var(x[-outliers,]), outliers = outliers) 
    } else {
      resultV <- sapply (data_v, function(x,outliers) sum(colVars(x[-outliers,])), outliers = outliers) 
    }
    result_matrixV <- rbind(result_matrixV, c(resultV, exp_data$id,projDim,0))
    
    write( c(resultR,exp_data$id,projDim,1),file = "~/results/distanceoutput.txt", ncolumns = 10, append = TRUE, sep = ";") 
    write( c(resultV,exp_data$id,projDim,0),file = "~/results/distanceoutput.txt", ncolumns = 10, append = TRUE, sep = ";") 
  }
  
  result_matrixR <- result_matrixR[2:nrow(result_matrixR),]
  result_matrixV <- result_matrixV[2:nrow(result_matrixV),]
  
  
    nalgorithms <- 7
    res_table <- result_matrixR[,1:nalgorithms]
    
    # Create Line Chart
    
    if (filename != FALSE)
    {
      pdf(paste("~/results/",filename,".pdf"))
    } else {
      pdf(paste("~/results/res_distance_c_",center,"_scale_",scale,"_",ds,".pdf"))
    }
    
    # get the range for the x and y axis 
    xrange <- range(1,maxfeature)
    yrange <- range(res_table) 
    
    # set up the plot 
    plot(xrange,yrange, type="n", xlab="Number of extracted features", ylab="Reconstruction Error" ) 
    colors <- rainbow(nalgorithms) 
    linetype <- c(1:nalgorithms) 
    plotchar <- seq(18,18+nalgorithms,1)
    
    # add lines 
    for (i in 1:nalgorithms) { 
      tree <- res_table[,i]
      lines(1:maxfeature, tree, type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i]) 
    } 
    
    # add a title and subtitle 
    title(ds)
    
    # add a legend 
    legend(xrange[2]*0.8, yrange[1]+ 0.9*(yrange[2]-yrange[1]), c("l2pca","PCAproj","PCAgrid","pcal1","l1pca","l1pcastar","l1pcahp"), cex=0.8, col=colors,pch=plotchar, lty=linetype)
    
    
    dev.off()
    
    
    
    res_table <- result_matrixV[,1:nalgorithms]
    
    # Create Line Chart
    
    if (filename != FALSE)
    {
      pdf(paste("~/results/",filename,".pdf"))
    } else {
      pdf(paste("~/results/res_variance_c_",center,"_scale_",scale,"_",ds,".pdf"))
    }
    
    # get the range for the x and y axis 
    xrange <- range(1,maxfeature)
    yrange <- range(res_table) 
    
    # set up the plot 
    plot(xrange,yrange, type="n", xlab="Number of extracted features", ylab="Variance in the features space" ) 
    colors <- rainbow(nalgorithms) 
    linetype <- c(1:nalgorithms) 
    plotchar <- seq(18,18+nalgorithms,1)
    
    # add lines 
    for (i in 1:nalgorithms) { 
      tree <- res_table[,i]
      lines(1:maxfeature, tree, type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i]) 
    } 
    
    # add a title and subtitle 
    title(ds)
    
    # add a legend 
    legend(xrange[2]*0.8, yrange[1]+ 0.9*(yrange[2]-yrange[1]), c("l2pca","PCAproj","PCAgrid","pcal1","l1pca","l1pcastar","l1pcahp"), cex=0.8, col=colors,pch=plotchar, lty=linetype)
    
    
    dev.off()
    
    
  if (filename != FALSE)
  {
    write.table(result_matrix, file = paste("~/results/",filename,".txt"), col.names = c("l2pca","PCAproj","PCAgrid","pcal1","l1pca","l1pcastar","l1pcahp", "Id dataset", "ProjDim"),sep = ";")
  } else {
    write.table(result_matrix, file = paste("~/results/res_distance_c_",center,"_scale_",scale,"_",ds,".txt"), col.names = c("l2pca","PCAproj","PCAgrid","pcal1","l1pca","l1pcastar","l1pcahp", "Id dataset", "ProjDim","Error or Var"),sep = ";")
  }
  
  
  
  
}