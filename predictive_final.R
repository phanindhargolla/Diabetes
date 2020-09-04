library("corrplot")
library("gmodels")
library("InformationValue")
library(pROC) 
install.packages("caret") 
install.packages("randomForest") 
install.packages("randomForestSRC") 
library("caret") 
library("randomForest") 
library("randomForestSRC") 
install.packages("rpart") 
library("rpart") 


db.data <- read.csv("C:\\Users\\Phani\\Downloads\\pima-indians-diabetes-database\\diabetes.csv") 
head(db.data) 
dim(db.data) 
summary(db.data) 

str(db.data) 
db.data$Outcome <- as.factor(db.data$Outcome)
#exploring the data
cor <- cor(db.data[,-9],method = "pearson")  
corrplot(cor) 

boxplot(Age~Outcome,data = db.data,col = c("blue","green"))
#splitting the data 

set.seed(12345) 
row.number <- sample(x = 1:nrow(db.data),size = 0.8 * nrow(db.data)) 
train.data <- db.data[row.number,]  
test.data <-  db.data[-row.number,]
rownames(train.data) <- 1:nrow(train.data)
rownames(test.data) <- 1:nrow(test.data)
head(train.data) 
head(test.data) 

dim(train.data) 

dim(test.data) 

#checking for equal proportion of outcome split
prop.table(table(train.data$Outcome))
prop.table(table(test.data$Outcome))

#logistic regression 
model1 <- glm(Outcome~.,data = train.data,family = binomial(link = "logit")) 
summary(model1) 

model2 <- glm(Outcome~Pregnancies+Glucose+BMI+DiabetesPedigreeFunction+Age,data = train.data,family = binomial(link = "logit")) 
summary(model2) 

da_pred1 <- predict(model1,newdata = test.data,type = "response") 
da_pre1 <- ifelse(da_pred1 > 0.5,1,0) 
da_obs1 <- test.data$Outcome 
probability1 <- mean(da_pre1 == da_obs1) 
probability1 

#confusion matrix
table(da_pre1,da_obs1) 
CrossTable(da_pre1,da_obs1) 

optCutoff <- optimalCutoff(da_obs1,da_pred1)[1] 

da_pre2 <- ifelse(da_pred1 > optCutoff,1,0) 
probability2 <- mean(da_pre2 == da_obs1,na.rm = TRUE) 
probability2 

table(da_pre2,da_obs1)
#confusionMatrix(da_pre2,da_obs1) 

da_pred3 <- predict(model2,newdata = test.data,type = "response") 
da_pre3 <- ifelse(da_pred3 > 0.5,1,0) 
probability3 <- mean(da_pre2 == da_obs1,na.rm = TRUE) 
probability3 
table(da_obs1,da_pre3)

roc_log <- roc(da_obs1,da_pre3) 
auc(roc_log)  
plot(roc_log)

#decision tree 

#db.data$Class.variable <- factor(db.data$Class.variable) 

dt<-train(factor(Outcome)~.,method = "rpart",data=train.data,control=rpart.control(minsplit=20)) 

#dt <- rpart(factor(Outcome) ~ ., data = train.data,model=TRUE) 

#dt$variable.importance 
#plot(dt) 
#text(dt) 

pred_des<-as.numeric(predict(dt,test.data)) 
pred_des<-ifelse(pred_des > optimalCutoff(da_obs1,pred_des),1,0) 

probability_des <- mean(pred_des == da_obs1) 
probability_des 
table(da_obs1,pred_des)

confusionMatrix(factor(pred_des),factor(da_obs1)) 
roc_des <- roc(da_obs1,pred_des) 
auc(roc_des)
plot(roc_des)

#Random Forests 

df <- train(Outcome~.,method = "rf",data = train.data) 
summary(df) 
print(df) 
#importance(df)   

#predicting 
pred_rf <- predict(df,test.data) 
pred_rf <- ifelse(pred_rf <- optimalCutoff(da_obs1,pred_rf),1,0) 

#accuracy 
probability_rf <- mean(pred_rf == da_obs1) 
probability_rf 
table(da_obs1,pred_rf)
confusionMatrix(factor(pred_rf),factor(da_obs1)) 
roc_rf <- roc(da_obs1,as.numeric(pred_rf)) 
auc(roc_rf)
plot(roc_rf)

#knn
train.data.knn <- train.data[,1:8]
test.data.knn <- test.data[,1:8]

train.data.labels <- train.data[,9]
test.data.labels <- test.data[,9]

#prediciting the output by using the KNN model
pred_knn <- knn(train = train.data.knn,test = test.data.knn,cl = train.data.labels, k = 21)

acc_knn <- mean(da_obs1 == pred_knn)
acc_knn
table(da_obs1,pred_knn)

roc_knn <- roc(da_obs1,as.numeric(pred_knn))
auc(roc_knn)
plot(roc_knn)
#CrossTable(x = da_obs1,y = pred_knn, prop.chisq = FALSE)


Algorithms <- c("Logistic","Decision Tree","Random Forest","knn") 
accuracies <- c(probability1,probability_des,probability_rf,acc_knn) 
final.plot <- data.frame(Algorithms,accuracies)  
ggplot(data = final.plot,aes(x = Algorithms,y = accuracies))+geom_bar(stat = "identity",fill = c("blue","red","green","violet")) 

auc_all <- c(auc(roc_log),auc(roc_des),auc(roc_rf),auc(roc_knn))
auc_plot <- data.frame(Algorithms,auc_all)  
ggplot(data = auc_plot,aes(x = Algorithms,y = auc_all))+geom_bar(stat = "identity",fill = c("blue","red","green","violet")) 
