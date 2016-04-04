  pcaTimeComp <- function(nrepeat = 10,obs = 1000, m = 10, p = 1, mu = 50, ntype = 1, percoutliers = 0.1, filename = FALSE)
{
    #import functions
    source("~/l1pcahp/src/data_generator.R") #dataset manager
    source("~/l1pcahp/src/l1pcahp.R")    #l1pcahp function
    source("~/l1pcahp/src/l2pca.R")   
    library("pcaL1")
    library("pcaPP")
    library("MASS")
    
    result_matrix <- rep(0, 13)
    

    for (k in 1:nrepeat)
    {
        
        #Data gen
        data_set <- dataGen(obs,m*2,as.integer(m),p,mu,ntype,percoutliers)
        
        #Shuffle
        nseed <-sample(10^5,1,replace=TRUE)
        set.seed(nseed)
        gp <- runif(obs)
        data_set <- data_set[order(gp),]
        ##### PCA #####
        timel2pca     <- system.time(data_l2pca <- l2pca(data_set,projDim = m,center = TRUE, scores = TRUE, projPoints = FALSE))
        timePCAproj   <- system.time(data_PCAproj <- PCAproj(data_set,m))
        timePCAgrid   <- system.time(data_PCAgrid <- PCAgrid(data_set,m))
        timepcaL1     <- system.time(data_pcal1 <- pcal1(data_set,projDim = m,center = TRUE, scores = TRUE, projPoints = FALSE, initialize = "l2pca"))
        timel1pca     <- system.time(data_l1pca <- l1pca(data_set,projDim = m,center = TRUE, projPoints = FALSE, initialize = "l2pca", tolerance = 0.0001, iterations = 10))
        timestar      <- system.time(data_l1pcastar <- l1pcastar(data_set,projDim = m,center = TRUE, scores = TRUE, projPoints = FALSE))
        timehp        <- system.time(data_l1plcahp <- l1pcahp(data_set,projDim = m, center = TRUE, scores = TRUE,  initialize = "l2pca", threshold = 0.01))

        data_time <- c(timel2pca[3],timePCAproj[3],timePCAgrid[3],timepcaL1[3], timel1pca[3], timestar[3],timehp[3])
        result_matrix <- rbind(result_matrix, c(data_time, obs,m,p,mu,ntype,percoutliers))

        write( c(data_time, obs,m,p,mu,ntype,percoutliers),file = "~/results/syntetic_time comparison.txt", ncolumns = 13, append = TRUE, sep = ";") 
      
    }
    result_matrix <- result_matrix[2:nrow(result_matrix),]
    
    if (filename != FALSE)
    {
       #write.table(result_matrix, file = paste("~/results/",filename,".txt"), col.names = c("seed","l2pca","PCAproj","PCAgrid","pcal1","l1pca","l1pcastar","l1pcahp","Time l2pca","Time PCAproj","Time PCAgrid","Time pcal1","Time l1pca","Time l1pcastar","Time l1pcahp", "Id dataset", "ProjDim"),sep = ";")
    } else {
      write.table(result_matrix, file = paste("~/results/time_comparison_",obs,"_m_",m,"_p_",p,"_type_",ntype,".txt"),sep = ";")
      }
    
    
    
    
  }