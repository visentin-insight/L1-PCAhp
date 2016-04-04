pcaOutliers <- function(ds, maxfeature, minfeature = 1, outliers, filename = FALSE, center = "mean", scale = "svd")
{
  #import functions
  source("~/l1pcahp/src/class_data.R") #dataset manager
  source("~/l1pcahp/src/l1pcahp.R")    #l1pcahp function
  source("~/l1pcahp/src/l2pca.R")    #10cvknn algorithm
  library("pcaL1")
  library("pcaPP")
  library("MASS")
  #pcaOutliers("milk",3,1,c(70,1,2,41))
  
  
  projection <- function(loadings, data) {
    result <- as.matrix(t((loadings) %*% t(loadings) %*% t(data)))
  }
  
  result_matrix <- rep(0, 9)
  
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
    
    data_r <- list(projection(data_l2pca$loadings,data_set), projection(data_PCAproj$loadings,data_set), projection(data_PCAgrid$loadings,data_set), projection(data_pcal1$loadings,data_set), projection(data_l1pca$loadings,data_set), projection(data_l1pcastar$loadings[,1:projDim],data_set), projection(data_l1plcahp$loadings,data_set))
  
        # apply the classificator to the dataset reduced
    resultD <- sapply (data_r, function(x,y,outliers) sum(abs(x[-outliers,]-y[-outliers,])), y = data_set, outliers = outliers) 
    result_matrix <- rbind(result_matrix, c(resultD, exp_data$id,projDim))
    
    write( c(resultD,exp_data$id,projDim),file = "~/results/distanceoutput.txt", ncolumns = 9, append = TRUE, sep = ";") 
  }
  
  result_matrix <- result_matrix[2:nrow(result_matrix),]
  
  if (p != 0)
  {
    nalgorithms <- 7
    res_table <- result_matrix[,1:nalgorithms]
    
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
    plot(xrange,yrange, type="n", xlab="Number of extracted features", ylab="Sum of residuals" ) 
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
    
  }
  
  if (filename != FALSE)
  {
    write.table(result_matrix, file = paste("~/results/",filename,".txt"), col.names = c("l2pca","PCAproj","PCAgrid","pcal1","l1pca","l1pcastar","l1pcahp", "Id dataset", "ProjDim"),sep = ";")
  } else {
    write.table(result_matrix, file = paste("~/results/res_distance_c_",center,"_scale_",scale,"_",ds,".txt"), col.names = c("l2pca","PCAproj","PCAgrid","pcal1","l1pca","l1pcastar","l1pcahp", "Id dataset", "ProjDim"),sep = ";")
  }
  
  
  
  
}