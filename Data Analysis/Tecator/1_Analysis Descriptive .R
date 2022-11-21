
# Librerias ---------------
library("fda.usc") #Data Tecator
library("rrcov")   #MRCD (cov.mrcd)
library("dplyr")   #Manejar datos
library("rrcovHD") #Outliers
library(cramp)     #Test Equal Variance
#install_github("dnayyala/cramp")
library(GGally) #GGPARCOOD
library(ggplot2)


# Data -------------------------------------------------------------------

data("tecator")
names(tecator)

tecator_data<-as.data.frame(tecator$absorp.fdata$data)
tecator_data$Fat20<-as.character(ifelse(tecator$y$Fat < 20, 0, 1))
View(tecator_data)

Tecator_fat20<-tecator_data %>% filter(Fat20==0)
Tecator_fat20<-select(Tecator_fat20, -Fat20 )
dim(Tecator_fat20)  #138

Tecator_FATT20<-tecator_data %>% filter(Fat20==1)
Tecator_FATT20<-select(Tecator_FATT20, -Fat20 )
dim(Tecator_FATT20) #77


# Description -------------------------------------------------------------
absorp <- tecator$absorp.fdata
Fat20 <- ifelse(tecator$y$Fat < 20, 0, 1) * 6 + 6
plot(tecator$absorp.fdata, col = Fat20,las = 1)

absorp.d1 <- fdata.deriv(absorp, nderiv = 1)
plot(absorp.d1, col = Fat20)

#WITH COLORS

ggparcoord(data = tecator_data,
           columns = 1:100,
           groupColumn = "Fat20",  alphaLines = 0.2) +
  scale_color_manual(
    values = c("darkorchid1", "midnightblue")
  )


# Derived Tecator Data ----------------------------------------------------

derivTecator<-as.data.frame(absorp.d1$data)
derivTecator$Fat20<-as.character(ifelse(tecator$y$Fat < 20, 0, 1))


ggparcoord(data = derivTecator,
           columns = 1:100,
           groupColumn = "Fat20",  alphaLines = 0.5) +
  scale_color_manual(
    values = c("darkorchid1", "red")
  )


Tecator_fat20_deriv<-derivTecator %>% filter(Fat20==0)
Tecator_fat20_deriv<-select(Tecator_fat20_deriv, -Fat20 )
dim(Tecator_fat20_deriv)  #138

Tecator_FATT20_deriv<-derivTecator %>% filter(Fat20==1)
Tecator_FATT20_deriv<-select(Tecator_FATT20_deriv, -Fat20 )
dim(Tecator_FATT20_deriv) #77

# Normal data -------------------------------------------------------------
# Detecting outliers ------------------------------------------------------
outliers <- OutlierPCOut(Fat20~.,data=tecator_data)
outliers


# Equal Covariance Test ---------------------------------------------------
#library(devtools)
test1<-cramp.statistic(Tecator_fat20,Tecator_FATT20, m=2)
test1



# Derivada data -------------------------------------------------------------
# Detecting outliers ------------------------------------------------------
outliers <- OutlierPCOut(Fat20~.,data=derivTecator)
outliers


# Equal Covariance Test ---------------------------------------------------
#library(devtools)
test1<-cramp.statistic(Tecator_fat20_deriv,Tecator_FATT20_deriv, m=5)
test1
?cramp.statistic


library(mvtnorm)
x = rmvnorm(n = 20, mean = numeric(1000), sigma = diag(runif(1000)))
y = rmvnorm(n = 20, mean = numeric(1000), sigma = diag(runif(1000)))
cramp.statistic(x, y, m = 5)


r<-c(outliers[1])
r
tecator_data[outliers,]

str(tecator_data)

tecator_data$Outlier<-ifelse(outliers@flag== 0, 1, 0)
tecator_data$Classification<-Fat20LDA


OutliersData <- tecator_data%>% filter(Outlier==1)
OutliersData$Classification
OutliersData$Fat20<-as.numeric(OutliersData$Fat20)


tecator_data %>% group_by(Fat20) %>% summarise(OutlierNum=sum(Outlier))

Compare<-cbind(var1=OutliersData$Classification,var2=OutliersData$Fat20)
Compare



