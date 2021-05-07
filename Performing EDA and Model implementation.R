library(pastecs)
library(ggplot2)
library(car)
library(caTools)
library("randomForest")
library(rpart)
library(rpart.plot)
library(caret)
library(ROSE)
library(ROCR)
library(dplyr)

data=read.csv("D:/Harini(christ unniversity)/2nd sem subjects/R/DublinTest dataset.csv")
head(data)
dim(data)
str(data)
summary(data)
stat.desc(data)
colSums(is.na(data))#checking if their are any null values
data=transform(data, Outcome=as.factor(Outcome))
ggplot(data) +aes(x = BloodPressure) +geom_histogram()+labs(title="BloodPressure")
ggplot(data) +aes(x = RBS) +geom_histogram()+labs(title="RBS")
ggplot(data) +aes(x = FBS) +geom_histogram()+labs(title="FBS")
ggplot(data) +aes(x = Serum.Insulin) +geom_histogram()+labs(title="Serum.Insulin")
ggplot(data) +aes(x = BMI) +geom_histogram()+labs(title="BMI")
ggplot(data) +aes(x = BUN) +geom_histogram()+labs(title="BUN")
ggplot(data) +aes(x = Age) +geom_histogram()+labs(title="Age")

ggplot(data) +aes(x = Outcome, y = BloodPressure) +geom_boxplot()
ggplot(data) +aes(x = Outcome, y = RBS) +geom_boxplot()
ggplot(data) +aes(x = Outcome, y = FBS) +geom_boxplot()
ggplot(data) +aes(x = Outcome, y = Serum.Insulin) +geom_boxplot()
ggplot(data) +aes(x = Outcome, y = BMI) +geom_boxplot()
ggplot(data) +aes(x = Outcome, y = BUN) +geom_boxplot()
ggplot(data) +aes(x = Outcome, y = Age) +geom_boxplot()


ggplot(data) +aes(x = Age, y = BloodPressure, colour = Outcome) +geom_point() +scale_color_hue()+labs(title="Age Vs. BloodPressure")
ggplot(data) +aes(x = Age, y = RBS, colour = Outcome) +geom_point() +scale_color_hue()+labs(title="Age Vs. RBS")
ggplot(data) +aes(x = Age, y = FBS, colour = Outcome) +geom_point() +scale_color_hue()+labs(title="Age Vs. FBS")
ggplot(data) +aes(x = Age, y = Serum.Insulin, colour = Outcome) +geom_point() +scale_color_hue()+labs(title="Age Vs. Serum.Insulin")
ggplot(data) +aes(x = Age, y = BMI, colour = Outcome) +geom_point() +scale_color_hue()+labs(title="Age Vs. BMI")
ggplot(data) +aes(x = Age, y = BUN, colour = Outcome) +geom_point() +scale_color_hue()+labs(title="Age Vs. BUN")

# Draw points on the qq-plot:
qqnorm(data$BloodPressure)
# Draw the reference line:
qqline(data$BloodPressure)

# Draw points on the qq-plot:
qqnorm(data$RBS)
# Draw the reference line:
qqline(data$RBS)

# Draw points on the qq-plot:
qqnorm(data$FBS)
# Draw the reference line:
qqline(data$FBS)

# Draw points on the qq-plot:
qqnorm(data$Serum.Insulin)
# Draw the reference line:
qqline(data$Serum.Insulin)

# Draw points on the qq-plot:
qqnorm(data$BMI)
# Draw the reference line:
qqline(data$BMI)

# Draw points on the qq-plot:
qqnorm(data$BUN)
# Draw the reference line:
qqline(data$BUN)

# Draw points on the qq-plot:
qqnorm(data$Age)
# Draw the reference line:
qqline(data$Age)

ggplot(data) +aes(x = BloodPressure) +geom_density()
ggplot(data) +aes(x = RBS) +geom_density()
ggplot(data) +aes(x = FBS) +geom_density()
ggplot(data) +aes(x = Serum.Insulin) +geom_density()
ggplot(data) +aes(x = BMI) +geom_density()
ggplot(data) +aes(x = BUN) +geom_density()
ggplot(data) +aes(x = Age) +geom_density()

table(data$Outcome)

sample=sample.split(data$Outcome,SplitRatio=0.75)
train=subset(data,sample==TRUE)
test=subset(data,sample==FALSE)

dim(train)
table(train$Outcome)
prop.table(table(train$Outcome))

#over sampling
train<- ovun.sample(Outcome ~ ., data = train, method = "over",N = 650)$data
table(train$Outcome)

lmModel=glm(Outcome~.,family=binomial,data=train)
summary(lmModel)
pred<-predict(lmModel,test,type="response")
s_pred_num <- ifelse(pred > 0.5, 1, 0)
s_pred <- factor(s_pred_num, levels=c(0, 1))
s_pred
pscl::pR2(lmModel)["McFadden"]
caret::varImp(lmModel)
car::vif(lmModel) #Checking for multicollinearity
anova(lmModel, test="Chisq")





confusionMatrix(table(train[,8], train$Outcome))
confusionMatrix(table(s_pred, test$Outcome))


#Random Forest
rf=randomForest(Outcome~.,data=train)
plot(rf)
varImpPlot(rf,sort = T,main = "Variable Importance",n.var = 3)
var.imp <- data.frame(importance(rf,type = 2))
var.imp
library(e1071)
rf_pred<-predict(rf,test,type="response")


confusionMatrix(table(train[,8], train$Outcome))
confusionMatrix(table(rf_pred, test$Outcome))


library(ROCR)
ROCRpred <- prediction(pred, test$Outcome)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))
auc<-performance(ROCRpred,measure="auc")
auc<-auc@y.values[[1]]
auc