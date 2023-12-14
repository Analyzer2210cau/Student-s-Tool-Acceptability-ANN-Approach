dataset1<-NULL

dataset1<-read.csv("F:/Sabina/Survey-Results/DataSet1.csv")
dataset1
colnames(dataset1)
dataset1<-dataset1[, c(9:29, 37)]
dataset1
str(dataset1)

dataset1[is.na(dataset1)]=3

dataset1

head(dataset1)
colnames(dataset1)
dataset1
#Normalization: Transforming data into min and max values 

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normlds <- as.data.frame(lapply(dataset1[, 1:21], normalize))

normlds
dataset1

colnames(dataset1)

dataset2<-dataset1[ c(22)]
dataset2
head(normlds)
colnames(dataset2)
ds<-cbind(dataset2, normlds)
class(ds)
head(ds)
rmse<-NULL
library("dplyr")
library(neuralnet)

attach(ds)

n.folds<-10
folds <- cut(sample(seq_len(nrow(ds))),  breaks=n.folds, labels=FALSE)

all.confusion.tables <- list()


for (i in seq_len(n.folds))
{
  
  
  train <-filter(ds, folds !=i)
  
  test <- filter(ds, folds==i)
  
  # set.seed(333)
  
  n<-neuralnet(SAC1~ PEOU1+PEOU2+PEOU3+PEOU5+PU1+PU2+
                 PU3+PU4+PU5+US1+US2+US3+US4+US5+US6+US7+QU1+QU2+QU3+QU4+QU5, 
               data=train, hidden=2, err.fct="ce", linear.output=FALSE)
  
  output<-compute(n, train[, -1])
  
  p1<-output$net.result
  
  pred1<-ifelse(p1>0.5, 1, 0)
  
  diff<-pred1-train$SAC1
 
  
  tab1<-table(pred1, train$SAC1)
  #Misclassfiication Error 
  ANN1[i, ]<-1-sum(diag(tab1))/sum(tab1)
  
  
}

rmse
ANN1
mean(ANN1)
