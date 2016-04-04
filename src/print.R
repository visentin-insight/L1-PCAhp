printPlot <- function()
{
  
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