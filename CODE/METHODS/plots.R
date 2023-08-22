library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

dat1 <- data.frame(
  class = factor(rep(c(0,1),each=10000)),
  weight1 = c(rnorm(10000,1.25,.75),rnorm(10000,-1.25,.75)),
  weight2 = c(rnorm(10000,1.25,.75),rnorm(10000,-1.25,.75)) )         

save(dat1, file = "DAT1.Rdata")

dat2 <- data.frame(
  class = factor(c(rep(0,14000),rep(1,6000))),
  weight1 = c(rnorm(14000,1.25,.75),rnorm(6000,-1.25,.75)),
  weight2 = c(rnorm(14000,1.25,.75),rnorm(6000,-1.25,.75)) ) 

save(dat2, file = "DAT2.Rdata")

dat3 <- data.frame(
  class = factor(c(rep(0,16000),rep(1,4000))),
  weight1 = c(rnorm(16000,1.25,.75),rnorm(4000,-1.25,.75)),
  weight2 = c(rnorm(16000,1.25,.75),rnorm(4000,-1.25,.75)) ) 

save(dat3, file = "DAT3.Rdata")

dat4 <- data.frame(
  class = factor(c(rep(0,18000),rep(1,2000))),
  weight1 = c(rnorm(18000,1.25,.75),rnorm(2000,-1.25,.75)),
  weight2 = c(rnorm(18000,1.25,.75),rnorm(2000,-1.25,.75)) ) 

save(dat4, file = "DAT4.Rdata")

dhs <- data.frame(mean = c(-0.01206638,-0.05400277, 0.5034106390010649),
                  method = c("dt","hd","svr"),
                  sd = c(0.1, 0.1, 0.1)
                  )

a <- ggplot(dat1, aes(x=weight1))+
  geom_density(aes(fill=class, color = class),alpha=.5,linewidth=0.7)+
  geom_vline(aes(xintercept = mean),data = dhs, col=c("deepskyblue4","orange2","deeppink3"), 
             linewidth=1)+
  #geom_segment(aes(x = mean-sd, y = 0.55, xend = mean+sd, yend = 0.55), 
               #linewidth=1,data = dhs, col = c("deepskyblue4","orange2","deeppink3"))+
  theme(legend.position = c(0.87, 0.85),
        legend.background = element_rect(fill = "white", color = "black"))+
  labs(x = "Feature", y = "Frequency")




a
dhs <- data.frame(mean = c(-0.2100777,-0.08311141,0.44746547417255966),
                  method = c("dt","hd","svr"),
                  sd = c(0.1, 0.1, 0.1)
)

b <- ggplot(dat2, aes(x=weight1))+
  geom_density(aes(fill=class, color = class, alpha=ifelse(class == 1, 0.45, 0.46)),linewidth=0.7)+
  geom_vline(aes(xintercept = mean),data = dhs, col=c("deepskyblue4","orange2","deeppink3"), 
             linewidth=1)+
  #geom_segment(aes(x = mean-sd, y = 0.55, xend = mean+sd, yend = 0.55), 
               #linewidth=1,data = dhs, col = c("deepskyblue4","orange2","deeppink3"))+
  theme(legend.position = c(0.87, 0.85),
        legend.background = element_rect(fill = "white", color = "black"))+
  labs(x = "Feature", y = "Frequency")



dhs <- data.frame(mean = c(-0.3623496,-0.359,0.49553839351846884),
                  method = c("dt","hd","svr"),
                  sd = c(0.1, 0.1, 0.1)
)

c <- ggplot(dat3, aes(x=weight1))+
  geom_density(aes(fill=class, color = class),alpha=0.2,linewidth=0.7)+
  geom_vline(aes(xintercept = mean),data = dhs, col=c("deepskyblue4","orange2","deeppink3"), 
             linewidth=1)+
  #geom_segment(aes(x = mean-sd, y = 0.55, xend = mean+sd, yend = 0.55), 
               #linewidth=1,data = dhs, col = c("deepskyblue4","orange2","deeppink3"))+
  theme(legend.position = c(0.87, 0.85),
        legend.background = element_rect(fill = "white", color = "black"))+
  labs(x = "Feature", y = "Frequency")

  
dhs <- data.frame(mean = c(-0.479358522678897,0.1832747,0.4612179163477005),
                  method = c("dt","hd","svr"),
                  sd = c(0.1, 0.1, 0.1)
)

d <- ggplot(dat4, aes(x=weight1))+
  geom_density(aes(fill=class, color = class),alpha=0.1,linewidth=0.7)+
  geom_vline(aes(xintercept = mean),data = dhs, col=c("deepskyblue4","orange2","deeppink3"), 
             linewidth=1)+
  #geom_segment(aes(x = mean-sd, y = 0.55, xend = mean+sd, yend = 0.55), 
               #linewidth=1,data = dhs, col = c("deepskyblue4","orange2","deeppink3"))+
  theme(legend.position = c(0.87, 0.85),
        legend.background = element_rect(fill = "white", color = "black"))+
  labs(x = "Feature", y = "Frequency")
a
b
c  
d  
