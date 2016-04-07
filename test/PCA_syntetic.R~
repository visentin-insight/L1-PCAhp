  pcaSyntetic <- function(nrepeat = 100,obs = 1000, m = 10, q = 2, p = 1, murange = c(25,50,75), ntype = 1, percoutliers = 0.1, filename = FALSE)
{
    #import functions
    source("~/l1pcahp/src/data_generator.R") #dataset manager
    source("~/l1pcahp/src/l1pcahp.R")    #l1pcahp function
    source("~/l1pcahp/src/l2pca.R")   
    library("pcaL1")
    library("pcaPP")
    library("matrixStats", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
    library("MASS")
    
    result_matrix <- rep(0, 15)
    result_matrixR <- rep(0, 15)
    
    projection <- function(loadings, data, q) {
      result <- abs(sum(t(t(( (loadings) %*% solve(t(loadings) %*% (loadings)) %*% t(loadings) )) %*% t(data_set) )[,1:q] - data_set[,1:q]))
    }
    
    for (k in 1:nrepeat)
    {
      for (mu in murange){
        
        #Data gen
        data_set <- dataGen(obs,m,q,p,mu,ntype,percoutliers)
        
        #Shuffle
        nseed <-sample(10^5,1,replace=TRUE)
        set.seed(nseed)
        gp <- runif(obs)
        data_set <- data_set[order(gp),]
        ##### PCA #####
        data_l2pca <- l2pca(data_set,projDim = q,center = FALSE, scores = TRUE, projPoints = FALSE)
        data_PCAproj <- PCAproj(data_set,q)
        data_PCAgrid <- PCAgrid(data_set,q)
        data_pcal1 <- pcal1(data_set,projDim = q,center = FALSE, scores = TRUE, projPoints = FALSE, initialize = "l2pca")
        data_l1pca <- l1pca(data_set,projDim = q,center = FALSE, projPoints = FALSE, initialize = "l2pca", tolerance = 0.001, iterations = 10)
        data_l1pcastar <- l1pcastar(data_set,projDim = q,center = FALSE, scores = TRUE, projPoints = FALSE)
        data_l1plcahp <- l1pcahp(data_set,projDim = q, center = FALSE, scores = TRUE,  initialize = "l2pca", threshold = 0.001)
       
        data_r <- list(data_l2pca$scores, data_PCAproj$scores, data_PCAgrid$scores,data_pcal1$scores,data_l1pca$scores,data_l1pcastar$scores,data_l1plcahp$scores)
        resultR <- sapply (data_r, function(x) sum(colVars(x))) 
        
        
        resultD <- c(projection(data_l2pca$loadings,data_set,q), projection(data_PCAproj$loadings,data_set,q), projection(data_PCAgrid$loadings,data_set,q), projection(data_pcal1$loadings,data_set,q), projection(data_l1pca$loadings,data_set,q), projection(data_l1pcastar$loadings[,1:q],data_set,q), projection(data_l1plcahp$loadings,data_set,q))
        
        # apply the classificator to the dataset reduced
        
        #proPoints <- sapply (data_r, function(x,y) t(x %*% y), y = t(data_set)) 
        #resultD <- sapply (proPoints, function(x,y) sum(abs(x[,1:q]-y[,1:q])), y = data_set) 
        
        result_matrix <- rbind(result_matrix, c(resultD, obs,m,q,p,mu,ntype,percoutliers,0))
        result_matrix <- rbind(result_matrix, c(resultR, obs,m,q,p,mu,ntype,percoutliers,1))
        
        write( c(resultD, obs,m,q,p,mu,ntype,percoutliers,0),file = "~/results/syntetic_stream.txt", ncolumns = 15, append = TRUE, sep = ";") 
        write( c(resultR, obs,m,q,p,mu,ntype,percoutliers,1),file = "~/results/syntetic_stream.txt", ncolumns = 15, append = TRUE, sep = ";") 
      }
    }
    result_matrix <- result_matrix[2:nrow(result_matrix),]
    if (TRUE)#p != 0)
    {
      
      res_table <- rep(0,7)
      for (mu in murange)
      {
        res_table <- rbind(res_table,colMeans(result_matrix[result_matrix[,12] == mu,1:7]))
      }
      res_table <- res_table[2:nrow(res_table),]
      
      
      # Create Line Chart
      
      
      if (filename != FALSE)
      {
        #pdf(paste("~/results/",filename,".pdf"))
      } else {
        pdf(paste("~/results/sync_obs_",obs,"_m_",m,"_q_",q,"_p_",p,"_type_",ntype,".pdf"))
      }
      nalgorithms <- 7
      
      # get the range for the x and y axis 
      xrange <- range(murange)
      yrange <- range(res_table) 
      
      # set up the plot 
      plot(xrange,yrange, type="n", xlab="Outlier magnitude", ylab= "L1 Distance" ) 
      colors <- rainbow(nalgorithms) 
      linetype <- c(1:nalgorithms) 
      plotchar <- seq(18,18+nalgorithms,1)
      
      # add lines 
      for (i in 1:nalgorithms) { 
        tree <- res_table[,i]
        lines(murange, tree, type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i]) 
      } 
      
      # add a title and subtitle 
      title(sub = paste("q = ",q,", p = ",p,", m = ",m,"."))
      
      # add a legend 
      legend(xrange[2]*0.8, yrange[1]+ 0.4*(yrange[2]-yrange[1]), c("l2pca","PCAproj","PCAgrid","pcal1","l1pca","l1pcastar","l1pcahp"), cex=0.8, col=colors,pch=plotchar, lty=linetype)
      
      
      dev.off()
    }
    
    
    if (filename != FALSE)
    {
       #write.table(result_matrix, file = paste("~/results/",filename,".txt"), col.names = c("seed","l2pca","PCAproj","PCAgrid","pcal1","l1pca","l1pcastar","l1pcahp","Time l2pca","Time PCAproj","Time PCAgrid","Time pcal1","Time l1pca","Time l1pcastar","Time l1pcahp", "Id dataset", "ProjDim"),sep = ";")
    } else {
      write.table(result_matrix, file = paste("~/results/sync_obs_rerror_",obs,"_m_",m,"_q_",q,"_p_",p,"_type_",ntype,".txt"),sep = ";")#, col.names = c("l2pca","PCAproj","PCAgrid","pcal1","l1pca","l1pcastar","l1pcahp","obs","m","q","p","mu","ntype","percoutliers"),sep = ";")
    }
    
    
    
    
  }