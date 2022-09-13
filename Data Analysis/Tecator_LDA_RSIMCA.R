library(robustbase) 
library(rrcovHD) #RSIMCA
library(rrcov) #MRCD
# LDA & QDA Tecator --------------------------------------------------------

#MRCD method
mrcdlda<-Linda(Fat20~., data=tecator_data, method="mrcd")
Fat20LDA<-predict(mrcdlda)@classification
predict(mrcdlda)



#MRCD method
mrcdlda_deriv<-Linda(Fat20~., data=derivTecator, method="mrcd")
Fat20LDA_deriv<-predict(mrcdlda_deriv)@classification
predict(mrcdlda_deriv)


#RSIMCA method
rs <- RSimca(Fat20~., data=tecator_data)
predict(rs)@classification
predict(rs)


#RSIMCA method
rs <- RSimca(Fat20~., data=derivTecator)
predict(rs)@classification
predict(rs)


plot(absorp.d1, col = Fat20LDA_deriv)
plot(tecator$absorp.fdata, col = Fat20LDA)


