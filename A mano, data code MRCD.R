# TESIS Código Preliminar MCDC  -------------------------------------------

#Packages

library(rrcov) #install.packages("rrcov") these package provides scalable robust estimators with high breakdown point and covers a large number of robustified multivariate analysis methods
library(ggplot2)
library(GGally)


# Bases de datos ----------------------------------------------------------
data(fruit)
str(fruit)


# Training Data -----------------------------------------------------------
#Train test D
trainingD1<- fruit[fruit$cultivar== "D", 1: 257]
nD<-dim(trainingD1)[1]*0.40
trainingD<- trainingD1[1:nD, 1:257]

#Train test M
trainingM1<- fruit[fruit$cultivar== "M", 1: 257]
nM<-dim(trainingM1)[1]*0.50
trainingM<- trainingM1[1:nM, 1:257]

#Train test  HA
trainingHA1<- fruit[fruit$cultivar== "HA", 1: 257]
nHA<-dim(trainingHA1)[1]*0.50
trainingHA<- trainingHA1[1:nHA, 1:257]

#Union Train Data
TrainData<-rbind(trainingD,trainingM,trainingHA)
str(TrainData)


# Test Data -----------------------------------------------------------

#Test Data D
TestD1<- fruit[fruit$cultivar== "D", 1: 257]
rD<-dim(TestD1)[1]*0.08
TestD<- TestD1[nD+1:rD+nD, 1:257]

#Test Data M
TestM1<- fruit[fruit$cultivar== "M", 1: 257]
rM<-dim(TestM1)[1]*0.10
TestM<- TestM1[nM+1:nM+rM, 1:257]

#Test Data  HA
TestHA1<- fruit[fruit$cultivar== "HA", 1: 257]
rHA<-dim(TestHA1)[1]*0.10 
TestHA<- TestHA1[rHA+1:rHA+nHA, 1:257]

#Union Test Data
TestData<-rbind(TestD,TestM,TestHA)
dim(TestData)


# MRCD Classification -----------------------------------------------------
mrcdlda<-Linda(cultivar~., data=TrainData, method="mrcd")


# Testing ----------------------------------------------------------------

#Predict of test Data Levels Classification
levelsPredict<-predict(mrcdlda, TestData[2:257])@classification

#Original Levels
levelsOriginal<-TestData[,1]


#Confution Matrix
Confusion<-as.data.frame(cbind(levelsOriginal,levelsPredict))



#Matrices de diferentes grupos
D=1
HA=2
M=3
Dpredict<-Confusion[Confusion$levelsOriginal == D,]
HApredict<-Confusion[Confusion$levelsOriginal == HA,]
Mpredict<-Confusion[Confusion$levelsOriginal == M,]

#Confusion Matrix
ConfusionMatrix<- matrix(NA,3,3)
colnames(ConfusionMatrix)<-c("D","HA","M")
rownames(ConfusionMatrix)<-c("D","HA","M")

for (i in 1:3) {
  ConfusionMatrix[1,i]<-mean(ifelse(Dpredict$levelsPredict == i,1,0))
  ConfusionMatrix[2,i]<-mean(ifelse(HApredict$levelsPredict == i,1,0))
  ConfusionMatrix[3,i]<-mean(ifelse(Mpredict$levelsPredict == i,1,0))
  
}

#Training Error
trainig_error <- mean(Confusion$levelsPredict != Confusion$levelsPredict) * 100
paste("trainig_error=",trainig_error,"%") #esto no ta bien ajja 


#Predict Graphic
UnionTest<-cbind(cultivar=levelsPredict,TestData[2:257])
ggparcoord(data = UnionTest,
           columns = 2:257,
           groupColumn = "cultivar",  alphaLines = 0.3) +
  scale_color_manual(
    values = c("darkorchid1", "midnightblue","green1")
  )

#Original Graphic
ggparcoord(data = TestData,
           columns = 2:257,
           groupColumn = "cultivar",  alphaLines = 0.3) +
  scale_color_manual(
    values = c("darkorchid1", "midnightblue","green1")
  )





#Graphic Union of test and training data
UnionData<-rbind(TrainData,TestData)
ggparcoord(data = UnionData,
           columns = 2:257,
           groupColumn = "cultivar",  alphaLines = 0.3) +
  scale_color_manual(
    values = c("darkorchid1", "midnightblue","green1")
  )




#BOX M'
library(heplots)
boxM(TrainData[2:257], TrainData$cultivar)


# MRCD Method -------------------------------------------------------------
mrcdlda<-Linda(cultivar~., data=TrainData, method="mrcd")
#Pruebas
predict(mrcdlda)@classification

