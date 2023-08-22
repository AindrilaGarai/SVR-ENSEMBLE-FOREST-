dat <- read.table("appendicitis.dat",sep=",")
head(dat)
table(dat$V8)/sum(table(dat$V8)) * 100
dim(dat)

dat <- read.table("ecoli.dat",sep=",")
head(dat)
dat$V8 <- ifelse(dat$V8 == " pp", 1, 0)
table(dat$V8)/sum(table(dat$V8)) * 100
dim(dat)

dat <- read.table("flare.dat",sep=",")
head(dat)
table(dat$V3)
(table(dat$V3)/sum(table(dat$V3)))*100
dim(dat)

dat <- read.table("segment0.dat",sep=",")
head(dat)
dat$V20 <- ifelse(dat$V20 == " positive", 1, 0)
table(dat$V20)/sum(table(dat$V20)) * 100
dim(dat)

dat <- read.csv("winequality-red.csv",sep=";")
head(dat)
dat$quality <- ifelse(dat$quality >= 7, 1, 0)
table(dat$quality)
table(dat$quality)/sum(table(dat$quality)) * 100
dim(dat)

dat <- read.csv("ac inflame.csv")
head(dat)
table(dat$X7)
(table(dat$X7)/sum(table(dat$X7)))*100

dat <- read.csv("Cleaned_data.csv")
head(dat)
table(dat$Severity)
(table(dat$Severity)/sum(table(dat$Severity)))*100

dat <- read.table("sonar.data",sep=",")
head(dat)
dat$V20 <- ifelse(dat$V20 == " positive", 1, 0)
table(dat$V61)/sum(table(dat$V61)) * 100
dim(dat)



dat <- read.table("dermatology.dat",sep=",")
head(dat)
table(dat$V11)
(table(dat$V11)/sum(table(dat$V11)))*100
dim(dat)

dat <- read.table("fertility_Diagnosis.txt",sep=",")
head(dat)
table(dat$V10)
dat$V10 <- ifelse(dat$V10 == "O",1,0)
table(dat$V10)/sum(table(dat$V10)) * 100
dim(dat)

dat <- read.table("page-blocks0.dat")
head(dat)
dat$V11 <- ifelse(dat$V11 == "positive",1,0)
table(dat$V11)
table(dat$V10)/sum(table(dat$V10)) * 100
dim(dat)

dat <- read.table("yeast-2_vs_4.dat",sep=",")
head(dat)
dat$V9 <- ifelse(dat$V9 == "positive",1,0)
table(dat$V9) * 100
dim(dat)

dat <- read.table("ecoli-0-2-3-4_vs_5.dat",sep=",")
head(dat)
table(dat$V8)
dat$V8 <- ifelse(dat$V8 == "positive",1,0)
(table(dat$V8)/sum(table(dat$V8)))*100
dim(dat)

dat <- read.table("yeast-0-3-5-9_vs_7-8.dat",sep=",")
head(dat)
table(dat$V9)
dat$V9 <- ifelse(dat$V9 == "positive",1,0)
(table(dat$V9)/sum(table(dat$V9)))*100
dim(dat)

dat <- read.table("ecoli-0-6-7_vs_5.dat",sep=",")
head(dat)
table(dat$V7)
dat$V7 <- ifelse(dat$V7 == "positive",1,0)
(table(dat$V7)/sum(table(dat$V7)))*100
dim(dat)

dat <- read.table("glass-0-1-4-6_vs_2.dat",sep=",")
head(dat)
table(dat$V10)
dat$V10 <- ifelse(dat$V10 == "positive",1,0)
(table(dat$V10)/sum(table(dat$V10)))*100
dim(dat)

dat <- read.table("glass2.dat",sep=",")
head(dat)
dat$V10 <- ifelse(dat$V10 == " positive",1,0)
table(dat$V10)
(table(dat$V10)/sum(table(dat$V10)))*100
dim(dat)

dat <- read.table("Imbalance-scale.data",sep=",")
head(dat)
dat$V5 <- ifelse(dat$V5 == "B",1 ,0)
table(dat$V5)
(table(dat$V5)/sum(table(dat$V5)))*100
dim(dat)

dat <- read.table("yeast-2_vs_8.dat",sep=",")
head(dat)
table(dat$V9)
dat$V9 <- ifelse(dat$V9 == " positive",1,0)
(table(dat$V9)/sum(table(dat$V9)))*100
dim(dat)
          
dat <- read.csv("car-vgood.csv")
head(dat)
table(dat$class)
dat$class <- ifelse(dat$class == "positive",1,0)
table(dat$class)/sum(table(dat$class)) *100
dim(dat)

dat <- read.table("yeast4.dat",sep=",")
head(dat)
table(dat$V9)

(table(dat$V9)/sum(table(dat$V9)))*100
dim(dat)

dat <- read.table("abalone-21_vs_8.dat",sep=",")
head(dat)
table(dat$V9)
(table(dat$V9)/sum(table(dat$V9)))*100
dim(dat)

dat <- read.table("abalone-20_vs_8-9-10.dat",sep=",")
head(dat)
table(dat$V9)
(table(dat$V9)/sum(table(dat$V9)))*100
dim(dat)

dat <- read.table("SPECTF.train",sep=",")
head(dat)
table(dat$V1)
table(dat$V1)/sum(table(dat$V1))
dim(dat)

dat <- read.table("hill valley.data",sep=",")
head(dat)
dat <- dat[-1,]
table(dat$V101)
table(dat$V101)/sum(table(dat$V101))
dim(dat)

dat <- read.csv("heart.csv")
head(dat)
table(dat$target)
table(dat$target)/sum(table(dat$target))
dim(dat)

dat <- read.table("monk-2.dat",sep=",")
head(dat)
table(dat$V7)
table(dat$V7)/sum(table(dat$V7))
dim(dat)

dat <- read.csv("movement_libras.csv",header = FALSE)
head(dat)
dat$V91 <- ifelse(dat$V91 <= 7, 1, 0)
table(dat$V91)

dat <- read.table("australian.dat",sep=",")
head(dat)
table(dat$V15)
table(dat$V15)/sum(table(dat$V15))
dim(dat)

dat <- read.table("crx.dat",sep=",")
head(dat)
table(dat$V16)
table(dat$V16)/sum(table(dat$V16))
dim(dat)

dat <- read.csv("cylinder-bands_bands.csv", header = FALSE)
head(dat)
table(dat$V40)
table(dat$V40)/sum(table(dat$V40))
dim(dat)

dat <- read.table("bupa.dat",sep=",")
head(dat)
table(dat$V7)
table(dat$V7)/sum(table(dat$V7))
dim(dat)

dat <- read.csv("Data_User_Modeling.csv",header = FALSE)
head(dat)
dat$V6 
table(dat$V6)
#LOW+VERYLOW
#HIGH+MIDDLE
table(dat$class)/sum(table(dat$class)) *100
dim(dat)
83+24
63+88

107/258

dat <- read.table("housevotes.dat",sep=",")
head(dat)
table(dat$V17)
table(dat$V17)/sum(table(dat$V17))
dim(dat)

dat <- read.table("breast-wisconsin.data",sep=",")
head(dat)
table(dat$V2)
table(dat$V2)/sum(table(dat$V2))
dim(dat)

dat <- read.table("ionosphere.dat",sep=",")
head(dat)
dat$V34 <- ifelse(dat$V34 == " b",1,0)
table(dat$V34)
table(dat$V34)/sum(table(dat$V34))
dim(dat)

mean(dat$V34)/sd(dat$V34)

dat <- read.table("wisconsin.dat",sep=",")
head(dat)
dat$V10 <- dat$V10 -2
table(dat$V10)
(table(dat$V10)/sum(table(dat$V10)))*100
dim(dat)

dat <- read.table("tic-tac-toe.dat",sep=",")
head(dat)
dat$V10 <- ifelse(dat$V10 == " positive", 1, 0 )
table(dat$V10)
table(dat$V10)/sum(table(dat$V10))
dim(dat)

dat <- read.table("car.dat",sep=",")
head(dat)

table(dat$V7)
(table(dat$V7)/sum(table(dat$V7)))*100
dim(dat)

dat <- read.csv("Indian Liver Patient Dataset (ILPD).csv", header = FALSE)
head(dat)
dat$V11 <- dat$V11 -1
table(dat$V11)
table(dat$V11)/sum(table(dat$V11))
dim(dat)

dat <- read.table("haberman.dat",sep=",")
head(dat)
table(dat$V4)
dat$V4 <- ifelse(dat$V4 == " positive", 1, 0 )
(table(dat$V4)/sum(table(dat$V4)))*100
dim(dat)

dat <- read.table("vehicle3.dat",sep=",")
head(dat)
dat$V19 <- ifelse(dat$V19 == " positive", 1, 0 )
table(dat$V19)/sum(table(dat$V19)) * 100
dim(dat)

dat <- read.table("parkinsons.data",sep=",")
head(dat)
dat <- dat[-1,]
table(dat$V18)
table(dat$V18)/sum(table(dat$V18))
dim(dat)

dat <- read.table("transfusion.data",sep=",")
head(dat)
table(dat$V5)
table(dat$V5)/sum(table(dat$V5))
dim(dat)

dat <- read.table("hayes-roth.data",sep=",")
dat$V6 <- ifelse(dat$V6 == 3,1,0)
head(dat)
table(dat$V6)

dat <- read.table("sonar.data",sep=",")
head(dat)
dat$V61 <- ifelse(dat$V61=="R",1,0)
mean(dat$V61)/sd(dat$V61)

dat <- read.table("heart-h.data",sep=",")
head(dat)
dat$V1 <- ifelse(dat$V61=="R",1,0)
mean(dat$V14)/sd(dat$V14)

dat <- read.csv("ac inflame.csv",sep=",")
head(dat)
dat$V1 <- ifelse(dat$V61=="R",1,0)
mean(dat$X7)/sd(dat$X7)

dat <- read.csv("Data_User_Modeling.csv",sep=",",header = FALSE)
head(dat)
table(dat$V6)
dat$V1 <- ifelse(dat$V61=="R",1,0)
mean(dat$X7)/sd(dat$X7)


dat <- read.table("auto-mpg.data")
head(dat)
dat <- dat[,-9]
dat[,4] <- as.numeric(dat[,4])
dat <- na.omit(dat)

testrows = sample(1:nrow(dat),replace=F,size=0.3*nrow(dat))
train = dat[-testrows,]
test = dat[testrows,]

library(tree)
library(Metrics)
library (randomForest)
tree.dat <- tree (V1 ~ ., train)
rf.dat <- randomForest(V1 ~ .,train, mtry=40,importance=TRUE)
importance(rf.dat)
varImpPlot(rf.dat)

train <- train[,c(1,4,3,7)]
test <- test[,c(1,4,3,7)]
tree.dat <- tree (V1 ~ ., train)
ypred <- predict (tree.dat , newdata = test)

head(ypred)
rmse(test$V1,ypred)
mae(test$V1,ypred)
rss <- sum((test$V1-ypred)^2)
tss <- sum((test$V1-mean(test$V1))^2)
r2 <- 1-(rss/tss)
adr2 <- 1-((rss/(117-3))/(tss/(117-1)))


library(readxl)
library(xlsx)
dat <- read_excel("Concrete_Data.xls")
dat <- read.xlsx("Concrete_Data.xls")
head(dat)

dat <- read.table("communities.data",sep = ",")
head(dat)

rf <- randomForest(V1 ~. , train,mtry=50)
pr <- predict(rf, newdata=test)
mae(test$V1,pr)
rmse(test$V1,pr)
mape(test$V1,pr)
rss <- sum((test$V1-pr)^2)
tss <- sum((test$V1-mean(test$V1))^2)
r2 <- 1-(rss/tss)
adr2 <- 1-((rss/(117-3))/(tss/(117-1)))
r2
adr2
