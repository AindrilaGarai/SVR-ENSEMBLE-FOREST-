variance <- function(y){
  if(length(y) <= 1) return(0)
  var(y)
}

information_gain <- function(y,mask,func=variance){
  s1 = sum(mask)
  s2 = length(mask)-s1
  if ( s1 == 0 | s2 == 0) return(0)
  func(y)-s1/(s1+s2)*func(y[mask])-s2/(s1+s2)*func(y[!mask])
}

sv_ratio = function(X, feature, val) # x= whole data, feature= which feature , mask= same feature region
{
  
  d <- dim(X)
  for (i in 1:d[2]) {
    take <- sum(X[,i] == feature)
    if(take == d[1]){
      f <- i
    }
  }
  
  new_data <- X[,-f] #78767768
  d <- dim(new_data)[2]
  
  ran <- numeric(d+1)
  ran[d+1] <- val-min(feature) # length of feature
  
  for(i in 1:d)
  { ran[i] <- max(X[,i]) - min(X[,i]) }
  
  if(ran[d+1] == 0)
  { 
    volume <- prod(ran[-(d+1)])
    
    sur <- numeric(length=d-1)
    for(j in 1:d-1)
    {
      term <- 0
      for(k in (j+1):d) 
      {
        term <- term + ran[j]*ran[k]
        sur[j] <- term
      }
    }
    surface <- 2*sum(sur)
  }
  else { volume <- prod(ran)
  
  sur <- numeric(length=d)
  for(j in 1:d)
  {
    term <- 0
    for(k in (j+1):(d+1)) 
    {
      term <- term + ran[j]*ran[k]
      sur[j] <- term
    }
  }
  surface <- 2*sum(sur)
  
  } 
  rtn <- surface/volume
  return(rtn)
}

max_information_gain_split <- function(y,x,X,func=variance){
  best_change = NA
  split_value = NA
  is_numeric = !(is.factor(x)|is.logical(x)|is.character(x))
  for( val in sort(unique(x))){
    mask <- x == val
    if (is_numeric) mask <- x < val
    change <- information_gain(y,mask,func) + (0.001* sv_ratio(X, x, val)) 
    if(is.na(best_change) | change > best_change){
      best_change = change
      split_value = val
    }
  }
  return(list("best_change"=best_change,
              "split_value"=split_value,
              "is_numeric"=is_numeric))
}

print(unlist(max_information_gain_split(iris$Petal.Width,iris$Sepal.Length, iris[,1:4])))
print(unlist(max_information_gain_split(iris$Petal.Width,iris$Sepal.Width, iris[,1:4])))
print(unlist(max_information_gain_split(iris$Petal.Width,iris$Petal.Length, iris[,1:4])))

print(unlist(max_information_gain_split(iris$Petal.Width,iris$Sepal.Length)))
print(unlist(max_information_gain_split(iris$Petal.Width,iris$Sepal.Width)))
print(unlist(max_information_gain_split(iris$Petal.Width,iris$Petal.Length)))

var(iris$Petal.Width[iris$Sepal.Length<5.6])+var(iris$Petal.Width[iris$Sepal.Length>=5.6])
var(iris$Petal.Width[iris$Sepal.Width<3.4])+var(iris$Petal.Width[iris$Sepal.Width>=3.4])
var(iris$Petal.Width[iris$Petal.Length<3])+var(iris$Petal.Width[iris$Petal.Length>=3])

library(ggplot2)
df <- data.frame(feature=c("feature1","feature2","feature3","feature4","feature5","feature6","feature7","feature8","feature9","feature10"),impurity=c(0.3761517, 0.1265224, 0.4583495, runif(7,0,1)),
                 se=runif(10,0,0.1))
df
ggplot(data=df,aes(x=reorder(feature,impurity), y=impurity, fill=impurity))+
  geom_bar(stat = "identity", width=0.4, position = "dodge")+
  xlab("feature")+
  ylab("measure")+
  geom_errorbar(aes(ymin=impurity-se, ymax=impurity+se),width=.2,position = position_dodge(0.4))+
  theme_classic()


library(ggplot2)
df <- data.frame(feature=rep(c("feature1","feature2","feature3","feature4","feature5","feature6","feature7","feature8","feature9","feature10"),2),impurity=c(0.3761517, 0.1265224, 0.4583495, runif(17,0,1)),
                 metrics=c(rep("mse",10),rep("r2",10)),
                 se=runif(20,0,0.2))
df
ggplot(data=df,aes(x=reorder(feature,impurity), y=impurity, fill=metrics))+
  geom_bar(stat = "identity", width=0.4, position = "dodge")+
  xlab("feature")+
  ylab("measure")+
  theme_classic()+
  theme (axis.text.x = element_text (angle = 90))+
  geom_errorbar(aes(ymin=impurity-se, ymax=impurity+se),width=.2,position = position_dodge(0.4))


# feature i off thakle auc

mean(c(0.7585185185185185,0.8009854913769504,0.767862031207227,
       0.7089285714285715))
sd(c(0.7585185185185185,0.8009854913769504,0.767862031207227,
     0.7089285714285715))

mean(0.4561,0.4745)
sd(c(0.4561,0.4745))


library(randomForest)
dat <- read.csv("Indian Liver Patient Dataset (ILPD).csv", header = FALSE)
head(dat)
dat[,2] <- as.numeric(as.factor(dat[,2]))
dat <- na.omit(dat)
bag <- randomForest(V1 ~ . , data=dat, mtry=12, importance=TRUE)
bag
importance(bag)

a <- function()
{
  b <- numeric(4)
  for(i in 1:4)
  {
    sam <- sample(1:579, 579, replace = TRUE)
    d <- dat[sam,]
    b[i] <- max_information_gain_split(d$V11,d$V11, d, func = gini_impurity)$best_change
  }
  return(list("mean"=mean(b),
              "sd"=sd(b)))
}
a()

x <- c(0.01817986,0.002729939,0.03923941,0.04246211,0.03107116,0.03346191,0.03095251,
       0.004162897,0.01460818,0.0182011)
z <- (x-min(x))/(max(x)-min(x))
z # impurity

auc <- c(0.6605136, 0.55, 0.6008696, 0.6937653,0.6567338,
         0.6380494,0.6306826,0.6332316,0.6714243,0.6867581)
s <- c(0.008222146,0.002916424,0.01332935,0.01282851,0.00679663,0.004410273,0.008767442,0.001239187,0.004970828,0.009892722)
sauc <- c(0.05758565, 0.05488153, 0.01999184,0.04277617,0.01875075,
          0.04414363,  0.02471822,0.0301664,0.01449764,0.0252319)

df <- data.frame(feature=rep(c("feature1","feature2","feature3","feature4","feature5","feature6","feature7","feature8","feature9","feature10"),2),
                 yaxis=c(z,auc),
                 metrics=c(rep("impurity",10),rep("auc",10)),
                 se=c(s,sauc))
df  # check

ggplot(data=df,aes(x=reorder(feature,yaxis), y=yaxis, fill=metrics))+
  geom_bar(stat = "identity", width=0.4, position = "dodge")+
  xlab("feature")+
  ylab("")+
  theme_classic()+
  theme (axis.text.x = element_text (angle = 90))+
  geom_errorbar(aes(ymin=yaxis-se, ymax=yaxis+se),width=.2,position = position_dodge(0.4))+
  theme( legend.title=element_blank())




dat <- read.csv("winequality-red.csv",sep=";")
head(dat)
dat[,12] <- as.numeric(as.factor(dat[,12]))
dat <- na.omit(dat)
bag <- randomForest(quality ~ . , data=dat, mtry=12, importance=TRUE)
bag
importance(bag)

a <- function()
{
  b <- numeric(4)
  for(i in 1:4)
  {
    sam <- sample(1:1599, 1599, replace = TRUE)
    d <- dat[sam,]
    b[i] <- max_information_gain_split(d$quality,d$density, d, func = gini_impurity)$best_change
  }
  return(list("mean"=mean(b),
              "sd"=sd(b)))
}
a()

mean(c(0.7482638888888888,0.7562962962962964,
       0.7638111235535647,0.8059402158342087))
sd(c(0.7482638888888888,0.7562962962962964,
     0.7638111235535647,0.8059402158342087))

x <- c(0.007192363,0.02662852,0.03131899,0.004996855,0.05180188,0.004267811,0.02669918,
       1.265791,0.01448441,0.03427718,0.05921657)
z <- (x-min(x))/(max(x)-min(x))
z # impurity 

auc <- c(0.7590737,0.8118173,0.7779902,0.8184362,
         0.8042034,0.7931074,0.7297195,0.7880247,
         0.7969731,0.7933577,0.7685779)

s <- c(0.001630745,0.003112957,0.009561739,0.0008038462,0.0694087, 0.0007527779,0.0009499164,
       0.09761571,0.01167873,0.003990609,0.002420966)

sauc <- c(0.03807307,0.03232023,0.03450767,0.03548315,
          0.03224348,0.01649525,0.06603885,0.01773042,
          0.01901828,0.02192369,0.02570448)

df <- data.frame(feature=rep(c("feature1","feature2","feature3","feature4","feature5","feature6","feature7","feature8","feature9","feature10","feature11"),2),
                 yaxis=c(x,auc),
                 metrics=c(rep("impurity",11),rep("auc",11)),
                 se=c(s,sauc))
df # check
ggplot(data=df,aes(x=reorder(feature,yaxis), y=yaxis, fill=metrics))+
  geom_bar(stat = "identity", width=0.4, position = "dodge")+
  xlab("feature")+
  ylab("")+
  theme_classic()+
  theme (axis.text.x = element_text (angle = 90))+
  geom_errorbar(aes(ymin=yaxis-se, ymax=yaxis+se),width=.2,position = position_dodge(0.4))+
  theme( legend.title=element_blank())



x <- c(0.007192363,0.02662852,0.03131899,0.004996855,0.05180188,0.004267811,0.02669918,
       0.01448441,0.03427718,0.05921657)
z <- (x-min(x))/(max(x)-min(x))
z # impurity 

auc <- c(0.7590737,0.8118173,0.7779902,0.8184362,
         0.8042034,0.7931074,0.7297195,
         0.7969731,0.7933577,0.7685779)

s <- c(0.001630745,0.003112957,0.009561739,0.0008038462,0.0694087, 0.0007527779,0.0009499164,
       0.01167873,0.003990609,0.002420966)

sauc <- c(0.03807307,0.03232023,0.03450767,0.03548315,
          0.03224348,0.01649525,0.06603885,
          0.01901828,0.02192369,0.02570448)

df <- data.frame(feature=rep(c("feature1","feature2","feature3","feature4","feature5","feature6","feature7","feature8","feature9","feature10"),2),
                 yaxis=c(z,auc),
                 metrics=c(rep("impurity",10),rep("auc",10)),
                 se=c(s,sauc))
df # check
ggplot(data=df,aes(x=reorder(feature,yaxis), y=yaxis, fill=metrics))+
  geom_bar(stat = "identity", width=0.4, position = "dodge")+
  xlab("feature")+
  ylab("")+
  theme_classic()+
  theme (axis.text.x = element_text (angle = 90))+
  geom_errorbar(aes(ymin=yaxis-se, ymax=yaxis+se),width=.2,position = position_dodge(0.4))+
  theme( legend.title=element_blank())


dat <- read.csv("glass-0-1-4-6_vs_2.dat",sep=",",header=FALSE)
head(dat)
dat[,10] <- as.numeric(as.factor(dat[,10]))
dat <- na.omit(dat)
bag <- randomForest(quality ~ . , data=dat, mtry=12, importance=TRUE)
bag
importance(bag)

a <- function()
{
  b <- numeric(4)
  for(i in 1:4)
  {
    sam <- sample(1:106, 106, replace = TRUE)
    d <- dat[sam,]
    b[i] <- max_information_gain_split(d$V8,d$V7, d, func = gini_impurity)$best_change
  }
  return(list("mean"=mean(b),
              "sd"=sd(b)))
}
a()

dat <- read.table("appendicitis.dat",sep=",")
head(dat)




