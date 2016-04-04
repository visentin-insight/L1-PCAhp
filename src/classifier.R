
##### CLASSIFICATION FUNCTION #####
# it returns the score of the classification
knn10cv <- function(data_set,data_class) {
  #load an edited version of KNN (max ties increased)
  #libPaths( c( .libPaths(), "~/R/x86_64-redhat-linux-gnu-library/3.0/") )
  #library("class", lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.0/")
  require(class)
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(data.frame(data_set))),breaks=10,labels=FALSE)
  totAccuracy <- 0
  #Perform 10 fold cross validation
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    data_set_test <- data_set[testIndexes, ]
    data_set_train <- data_set[-testIndexes, ]
    data_class_test <- data_class[testIndexes]
    data_class_train <- data_class[-testIndexes]
    class_result <- knn(train = data.frame(data_set_train), test = data.frame(data_set_test), cl =data_class_train, k= 1,use.all = FALSE) #as.integer(sqrt(nrow(data_r))))
    confusionMx <- table(data_class_test,class_result)
    accuracy <- sum(diag(confusionMx))/sum(confusionMx)
    totAccuracy <- totAccuracy + accuracy
  }
  totAccuracy <- totAccuracy / 10
}
