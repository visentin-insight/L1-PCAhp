.libPaths( c( .libPaths(), "~/R/x86_64-redhat-linux-gnu-library/3.0/") )
classDataset <- function (dataname)
{
  ##UCI datasets "australian", "balance","breastcancer","dermatology", "ionsphere", "iris", "liver","sonar", "vehicle","yeast", "letter","glass","diabetes","mfeat","isolet"
  ##Outliers datasets "milk","pollution","artificial"
  dataset <- new.env()
  switch(dataname, 
         australian={
           ## australian DATASET
           data_full <- read.table("~/l1pcahp/data/australian.data", fileEncoding="UTF-8", sep=" ", dec = ".")
           dataset$attributes <- sapply(data_full[complete.cases(data_full),1:14],as.numeric)
           dataset$class <- data_full[complete.cases(data_full),15]
           dataset$id <- 1
         },
         balance={ 
           ## balance DATASET
           data_full <- read.table("~/l1pcahp/data/balance-scale.data", fileEncoding="UTF-8", sep=",")
           dataset$attributes <- sapply(data_full[complete.cases(data_full),2:5],as.numeric)
           dataset$class <- data_full[complete.cases(data_full),1]
           dataset$id <- 2
         },
         breastcancer={
           ## BreastCancer DATASET
           library(mlbench)
           data(BreastCancer)
           dataset$attributes <- sapply(BreastCancer[complete.cases(BreastCancer),2:10],as.numeric)
           dataset$class <- BreastCancer[complete.cases(BreastCancer),11]
           dataset$id <- 3
         },
         dermatology ={
           ## dermatology DATASET
           data_full <- read.table("~/l1pcahp/data/dermatology.data", fileEncoding="UTF-8", sep=",")
           dataset$attributes <- sapply(data_full[complete.cases(data_full),1:34],as.numeric)
           dataset$class <- data_full[complete.cases(data_full),35]
           dataset$id <- 4
         },
         heart={
           ## heartdisease DATASET
           data_full <- read.table("~/l1pcahp/data/processed.cleveland.data", fileEncoding="UTF-8", sep=",")
           dataset$attributes <- sapply(data_full[complete.cases(data_full),1:13],as.numeric)
           dataset$class <- data_full[complete.cases(data_full),14]
           dataset$id <- 5
         },
          ionosphere={
            ## Ionoshpere DATASET
            library(mlbench)
            data(Ionosphere)
            dataset$attributes <- Ionosphere[complete.cases(Ionosphere),3:34]
            dataset$class <- Ionosphere[complete.cases(Ionosphere),35]
            dataset$id <- 6
          },
         iris={
           ## IRIS DATASET
           dataset$attributes <- iris[,c(1,2,3,4)]
           dataset$class <- iris[,5]
           dataset$id <- 7
         },
         liver={
           ## liver DATASET
           data_full <- read.table("~/l1pcahp/data/bupa.data", fileEncoding="UTF-8", dec=".",sep =",")
           dataset$attributes <- sapply(data_full[complete.cases(data_full),1:6],as.numeric)
           dataset$class <- data_full[complete.cases(data_full),7]
           dataset$id <- 8
         },
          sonar={
            ## SONAR DATASET
            library(mlbench)
            data(Sonar)
            dataset$attributes <- Sonar[complete.cases(Sonar),1:60]
            dataset$class <- Sonar[complete.cases(Sonar),61]
            dataset$id <- 9
          },
         vehicle={
           ## Veichle DATASET
           library(mlbench)
           data(Vehicle)
           dataset$attributes <- Vehicle[complete.cases(Vehicle),1:18]
           dataset$class <- Vehicle[complete.cases(Vehicle),19]
           dataset$id <- 10
         },
         waveform={
           ## waveform DATASET
           data_full <- read.table("~/l1pcahp/data/waveform-+noise.data", fileEncoding="UTF-8", dec=".",sep =",")
           dataset$attributes <- sapply(data_full[complete.cases(data_full),1:21],as.numeric)
           dataset$class <- data_full[complete.cases(data_full),41]
           dataset$id <- 11
         },
         yeast ={
           ## yeast DATASET
           data_full <- read.table("~/l1pcahp/data/yeast.data", fileEncoding="UTF-8", dec=".")
           dataset$attributes <- sapply(data_full[complete.cases(data_full),2:9],as.numeric)
           dataset$class <- data_full[complete.cases(data_full),10]
           dataset$id <- 12
         },
         letter={
            ## Letter DATASET
            library(mlbench)
            data(LetterRecognition)
            dataset$attributes <- LetterRecognition[complete.cases(LetterRecognition),2:17]
            dataset$class <- LetterRecognition[complete.cases(LetterRecognition),1]
            dataset$id <- 13
         },
         glass={
           ## glass DATASET
           data_full <- read.table("~/l1pcahp/data/glass.data", fileEncoding="UTF-8", dec=".",sep =",")
           dataset$attributes <- sapply(data_full[complete.cases(data_full),2:10],as.numeric)
           dataset$class <- data_full[complete.cases(data_full),11]
           dataset$id <- 14
         },
         diabetes={
           ## diabetes DATASET
           data_full <- read.table("~/l1pcahp/data/pima-indians-diabetes.data", fileEncoding="UTF-8", dec=".",sep =",")
           dataset$attributes <- sapply(data_full[complete.cases(data_full),1:8],as.numeric)
           dataset$class <- data_full[complete.cases(data_full),9]
           dataset$id <- 15
         },
         mfeat={
           ## mfeat DATASET
           data_full <- read.table("~/l1pcahp/data/mfeat-fac.data", fileEncoding="UTF-8",sep =" ")
           dataset$attributes <- sapply(data_full[complete.cases(data_full[,2:217]),2:217],as.numeric)
           dataset$class <- c(rep(0,200),rep(1,200),rep(2,200),rep(3,200),rep(4,200),rep(5,200),rep(6,200),rep(7,200),rep(8,200),rep(9,200))
           dataset$id <- 16
         },
         isolet={
           ## isolet   DATASET
           data_full <- read.table("~/l1pcahp/data/isolet5.data", fileEncoding="UTF-8", dec=".",sep =",")
           dataset$attributes <- sapply(data_full[complete.cases(data_full),1:617],as.numeric)
           dataset$class <- data_full[complete.cases(data_full),618]
           dataset$id <- 17
         },
         milk={
           ##milk   DATASET
           library(rrcov)
           data(milk)
           dataset$attributes <- milk[-63,]
           dataset$id <- 18
         },
         pollution={
           ##pollution   DATASET
           data_full <- read.table("~/l1pcahp/data/pollution.data", fileEncoding="UTF-8", dec=".",sep =" ")
           dataset$attributes <- sapply(data_full[,2:17],as.numeric)
           dataset$id <- 19
         },
         artificial={
           ##artificial   DATASET
           library("wle")
           data(artificial)
           dataset$attributes <- artificial
           dataset$id <- 20
         }
  )
  dataset
  
}