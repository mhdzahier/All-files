breastcancer<- read.csv("/Users/zahiernasrudin/Downloads/data 2.csv", header = T, stringsAsFactors = FALSE)

#structure
str(breastcancer)

library(dplyr)
library(ggplot2)


#frequency of tumor
breastcancer %>%
  select(diagnosis)%>%
  group_by(diagnosis)%>%
  summarise(total=n())%>%
  ggplot(aes(x=diagnosis,y=total, fill=diagnosis))+
  geom_bar(stat = "identity")

#missing value

library(VIM)
miss<- aggr(breastcancer, col=c('lightblue2','indianred3'),
            numbers=TRUE,prop = F,labels=names(breastcancer), cex.axis=0.55, combined = TRUE, 
            ylab= "Combination of missing (red) and complete values(blue)",
            gap=2)

breastcancer <- breastcancer[,-33]

library(plyr)

library(psych)

#histogram

multi.hist(breastcancer[,sapply(breastcancer, is.numeric)][2:10], bcol = 'lightblue')
multi.hist(breastcancer[,sapply(breastcancer, is.numeric)][11:19], bcol = 'lightblue')
multi.hist(breastcancer[,sapply(breastcancer, is.numeric)][20:28], bcol = 'lightblue')
multi.hist(breastcancer[,sapply(breastcancer, is.numeric)][29:31], bcol = 'lightblue')

#skewness
library(fBasics)

continuous_variables<- breastcancer[,sapply(breastcancer, is.numeric)]
colSkewness(continuous_variables[-1])



#cube root transformation

normal_cube<-sign(continuous_variables[,c(4,11,
                                          12,13,14,
                                          15,16,17,18,19,
                                          20,21,25,31)]) * abs(continuous_variables[,c(4,11,
                                                                                       12,13,14,
                                                                                       15,16,17,18,19,
                                                                                       20,21,25,31)])^(1/3)

breastcancer_new<-cbind(continuous_variables[,c(1,2,3,5,
                                                6,7,8,9,10,
                                                22,23,24,
                                                26,27,28,29,30)],
                        normal_cube,breastcancer[,sapply(breastcancer,is.character)]) 

colnames(breastcancer_new)[32]<- "diagnosis"



#training and testing

library(caret)

set.seed(23564)
portion= createDataPartition(breastcancer_new$diagnosis, p=0.7, list = FALSE)
train= breastcancer_new[portion,]
test= breastcancer_new[-portion,]  

table(train$diagnosis)
table(test$diagnosis)

#cross validation

set.seed(236)
cvfolds= createMultiFolds(train$diagnosis,k=10)
cv.ctrl=trainControl(method = "repeatedcv",number = 10, repeats = 3,
                     index = cvfolds)
#modelling
#rpart

rpart.cv = train(diagnosis~., data= train[,-1], method="rpart",
                 trControl=cv.ctrl, tuneLength=7)

test_pred=predict(rpart.cv,newdata = test[,-1])
cm_rpart= confusionMatrix(test_pred, test$diagnosis)

#logistic

logistic.cv = train(diagnosis~., data= train[,-1], method="glm",
                 trControl=cv.ctrl, tuneLength=7)
test_pred_logistic=predict(logistic.cv, newdata= test[,-1])
cm_logistic= confusionMatrix(test_pred_logistic,test$diagnosis)

#naive bayes

naive_bayes.cv = train(diagnosis ~., data = train[,-1], method = "nb",
                    trControl=cv.ctrl,
                    tuneLength = 7)
test_pred_naivebayes=predict(naive_bayes.cv, newdata= test[,-1])
cm_naivebayes=confusionMatrix(test_pred_naivebayes,test$diagnosis)

#svmradial
svm_radial.cv <- train(diagnosis ~., data = train[,-1], method = "svmRadial",
             trControl=cv.ctrl,
             tuneLength = 7)
test_pred_radial<- predict(svm_radial.cv, newdata= test[,-1])
cm_svmradial=confusionMatrix(test_pred_radial,test$diagnosis)



#svmpoly
svm_poly.cv <- train(diagnosis ~., data = train[,-1], method = "svmPoly",
                   trControl=cv.ctrl,
                   tuneLength = 7)
test_pred_poly<- predict(svm_poly.cv, newdata= test[,-1])
cm_svmpoly=confusionMatrix(test_pred_poly,test$diagnosis)

#svmlinear

svmlinear.cv <- train(diagnosis ~., data = train[,-1], method = "svmLinear",
                   trControl=cv.ctrl,
                   tuneLength = 7)
test_pred_svmlinear<- predict(svmlinear.cv, newdata= test[,-1])
cm_svmlinear=confusionMatrix(test_pred_svmlinear,test$diagnosis)

#neuralnetwork

neural.cv <- train(diagnosis ~., data = train[,-1], method = "nnet",
                      trControl=cv.ctrl,
                      tuneLength = 7)
test_pred_neural<- predict(neural.cv, newdata= test[,-1])
cm_neural=confusionMatrix(test_pred_neural,test$diagnosis)



#random forest
library(party)
randomforest.cv <- train(diagnosis ~., data = train[,-1], method = "cforest",
                      trControl=cv.ctrl,
                      tuneLength = 7)
test_pred_randomforest<- predict(randomforest.cv, newdata= test[,-1])
cm_randomforest=confusionMatrix(test_pred_randomforest,test$diagnosis)


#plot accuracy

accuracy= data.frame(algorithm= c("RPART","LOGISTIC","NAIVE BAYES","SVM RADIAL",
                                  "SVM POLY", "SVM LINEAR","NEURAL NETWORK", "RANDOM FOREST"),
                     score= round(c(cm_rpart$overall[1]*100,cm_logistic$overall[1]*100
                              ,cm_naivebayes$overall[1]*100,cm_svmradial$overall[1]*100,
                              cm_svmpoly$overall[1]*100,cm_svmlinear$overall[1]*100,
                              cm_neural$overall[1]*100, cm_randomforest$overall[1]*100),2))

ggplot(data=accuracy, aes(x=algorithm, y=score, fill=algorithm)) +
  geom_bar(stat="identity")+
  labs(y="Accuracy")+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(label=score), position=position_dodge(width=0.9), vjust=-0.25)
