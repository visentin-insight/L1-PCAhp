compPCA <- function(ds, shuffN = 10, maxfeature, minfeature = 1,startseed = 12345, filename = FALSE, center = "mean", scale = "svd")
{
  #import functions
  source("~/l1pcahp/src/dataset_manager.R") #dataset manager
  source("~/l1pcahp/pca/l1pcahp.R")    #l1pcahp function
  source("~/l1pcahp/src/classifier.R")    #10cvknn algorithm
  source("~/l1pcahp/pca/l2pca.R")    #10cvknn algorithm
  library("pcaL1")
  library("pcaPP")
  library("MASS")
  
  result_matrix <- rep(0, 17)
  
  #shuffle the dataset
  
  exp_data <- classDataset(ds)
  data_set <- exp_data$attributes
  data_class <- exp_data$class
  if (center == "mean")
    data_set <- scale(data_set, center = TRUE, scale = FALSE)
  if (center == "median")
    data_set <- scale(data_set,center= apply(data_set, 2, FUN = median), scale = FALSE)
  
  if (scale == "svd")
    data_set <- scale(data_set, center = FALSE, scale = TRUE)
  if (scale == "MAD")
    data_set <- scale(data_set,center= FALSE, scale =apply(data_set,2, mad))
  
  for (k in 1:shuffN)
  {
    nseed <-sample(10^5,1,replace=TRUE)
    set.seed(nseed)
    gp <- runif(nrow(data_set))
    data_set <- data_set[order(gp),]
    data_class <- data_class[order(gp)]
    

    
    for (projDim in minfeature:maxfeature){
      ##### PCA #####
      timel2pca     <- system.time(data_l2pca <- l2pca(data_set,projDim = projDim,center = TRUE, scores = TRUE, projPoints = FALSE))
      timePCAproj   <- system.time(data_PCAproj <- PCAproj(data_set,projDim))
      timePCAgrid   <- system.time(data_PCAgrid <- PCAgrid(data_set,projDim))
      timepcaL1     <- system.time(data_pcal1 <- pcal1(data_set,projDim = projDim,center = TRUE, scores = TRUE, projPoints = FALSE, initialize = "l2pca"))
      timel1pca     <- system.time(data_l1pca <- l1pca(data_set,projDim = projDim,center = TRUE, projPoints = FALSE, initialize = "l2pca", tolerance = 0.0001, iterations = 10))
      timestar      <- system.time(data_l1pcastar <- l1pcastar(data_set,projDim = projDim,center = TRUE, scores = TRUE, projPoints = FALSE))
      timehp        <- system.time(data_l1plcahp <- l1pcahp(data_set,projDim = projDim, center = TRUE, scores = TRUE,  initialize = "l2pca", threshold = 0.001))
      
      data_r <- list(data_l2pca$scores, data_PCAproj$scores, data_PCAgrid$scores,data_pcal1$scores,data_l1pca$scores,data_l1pcastar$scores,data_l1plcahp$scores)
      data_time <- c(timel2pca[3],timePCAproj[3],timePCAgrid[3],timepcaL1[3], timel1pca[3], timestar[3],timehp[3])
      
      ##### KNN CLASSIFICATION #####  
      # apply the classificator to the dataset reduced
      resultC <- sapply (data_r, knn10cv, data_class = data_class) 
      result_matrix <- rbind(result_matrix, c(nseed,resultC, data_time,exp_data$id,projDim))
      
      write( c(nseed,resultC, data_time,exp_data$id,projDim),file = "~/results/classifieroutput.txt", ncolumns = 17, append = TRUE, sep = ";") 
    }
  }
  result_matrix <- result_matrix[2:nrow(result_matrix),]
  res_table <- colMeans(result_matrix[result_matrix[,17] == 1,2:8])
  for (i in 2:maxfeature)
  {
    res_table <- rbind(res_table,colMeans(result_matrix[result_matrix[,17] == i,2:8]))
  }
  
  # Create Line Chart
  
  
  if (filename != FALSE)
  {
    pdf(paste("~/results/",filename,".pdf"))
  } else {
    pdf(paste("~/results/res_",ds,"_",shuffN,".pdf"))
  }
  nalgorithms <- 7
  
  # get the range for the x and y axis 
  xrange <- range(1,maxfeature)
  yrange <- range(res_table) 
  
  # set up the plot 
  plot(xrange,yrange, type="n", xlab="Number of extracted features", ylab="Correct Classification Rate (%)" ) 
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
  legend(xrange[2]*0.8, yrange[1]+ 0.4*(yrange[2]-yrange[1]), c("l2pca","PCAproj","PCAgrid","pcal1","l1pca","l1pcastar","l1pcahp"), cex=0.8, col=colors,pch=plotchar, lty=linetype)
  
  
  dev.off()
  if (filename != FALSE)
  {
    write.table(result_matrix, file = paste("~/results/",filename,".txt"), col.names = c("seed","l2pca","PCAproj","PCAgrid","pcal1","l1pca","l1pcastar","l1pcahp","Time l2pca","Time PCAproj","Time PCAgrid","Time pcal1","Time l1pca","Time l1pcastar","Time l1pcahp", "Id dataset", "ProjDim"),sep = ";")
  } else {
    write.table(result_matrix, file = paste("~/results/res_",ds,"_",shuffN,".txt"), col.names = c("seed","l2pca","PCAproj","PCAgrid","pcal1","l1pca","l1pcastar","l1pcahp","Time l2pca","Time PCAproj","Time PCAgrid","Time pcal1","Time l1pca","Time l1pcastar","Time l1pcahp", "Id dataset", "ProjDim"),sep = ";")
  }
  
  
  
  
}
