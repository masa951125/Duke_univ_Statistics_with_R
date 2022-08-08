install.packages("tidyverse")
install.packages("statsr")
install.packages("dplyr")

library(tidyverse)
library(dplyr)
library(statsr)

yrbss <- read.csv("yrbss.csv")
IQR(yrbss$height, na.rm = T)
ggplot(data=yrbss, aes(x= height)) +geom_histogram()

mu_lst <-c()
for(i in 1:500){
  df <- yrbss %>% sample_n(size=i)
  mu <- mean(df$height,na.rm=T)
  mu_lst <- append(mu_lst, mu)
}

plot(mu_lst, type="l",xlab="sample size", ylab = "sample mean (height)")
abline(h=mean(yrbss$height, na.rm=T),col="red")

yrbss_samp <- yrbss %>%  sample_n(100)

yrbss_sample_mean_1000 <- yrbss %>% 
  rep_sample_n(size=100, rep=1000, replace=T)%>%
  summarize(x_bar_physical = mean(physically_active_7d, na.rm=T))



ggplot(data=yrbss_sample_mean_1000,aes(x=x_bar_physical))+ 
  geom_histogram(color="black")+ 
  geom_vline(xintercept=mean(yrbss_sample_mean_1000$x_bar_physical),col="red")

pt(-0.87,df=199,lower.tail = T)*2

#r_lab
#finding the smallest p-values

p <- vector()
df <- data.frame()

for(i in unique(nc$mage)){
  nc_test <-nc %>% mutate(test_mature= ifelse(nc$mage>i, "mature", "young"))
  d <- inference(y= weight, x=test_mature, data=nc_test, 
                 statistic = "mean", type = "ht", 
                 null = 0, alternative = "twosided", method = "theoretical")
  df <-rbind(df,c(i, d$p_value))
} 
colnames(df)  <- c("age","p_value")
df

stem <-read.csv("stem_cell.csv")
stem <-stem %>% mutate(diff=(after-before))

ctl <- stem %>% filter(stem$trmt=="ctrl")
trt <- stem %>% filter(stem$trmt=="esc")

ggplot(data= ctl) + 
  geom_histogram(aes(x= diff),binwidth = 1.5, color="black")

ggplot(data= trt) + 
  geom_histogram(aes(x= diff), binwidth = 1.5,color="black")

SE_diff <- sqrt(var(ctl$diff)/nrow(ctl) + var(trt$diff)/nrow(trt))
  
qt(0.025, df=8)

t.test(trt$diff,ctl$diff,  mu=0)
curve(dt(x,8),-5,5)
abline(v=qt(0.025,8),col="red")
abline(v=qt(0.975,8),col="red")

baby <- read.csv("babies.csv")
sample_baby <- sample_n(baby,150)
non_smoker <- sample_baby %>% filter(sample_baby$smoke==0)
smoker <- sample_baby %>% filter(sample_baby$smoke==1)

x_smoker <- mean(smoker$weight, na.rm=T)
x_nonsmoker <- mean(non_smoker$weight,na.rm=T)

t.test(smoker$weight, non_smoker$weight, mu=0)

ggplot(data= smoker) + geom_histogram(aes(x=weight),col="blue",alpha=0.5)
ggplot(data=non_smoker)+geom_histogram(aes(x=weight),col="red", alpha=0.5)

pt(1.54,df=49,lower.tail = F)*2
pt(1.15,df=26, lower.tail=F)*2

pnorm(-0.20)
pnorm(1.99)
qnorm(0.80)
qnorm(0.9)

#Analysis of Variance

bat10 <- read.csv("mlbbat10.csv")
bat10$position[(bat10$position=="1B")|(bat10$position=="2B")|(bat10$position=="3B") ]="IF"  
bat10$position <- as.factor(bat10$position)

bat10_test <- bat10 %>% 
  filter((bat10$position=="C")|(bat10$position=="DH")|(bat10$position=="IF")|(bat10$position=="OF"))

bat10_data <-bat10_test %>% group_by(position) %>% summarize(n=n(),mu= mean(obp),sd=sd(obp))

ggplot(data=bat10_test) + geom_boxplot(aes(position,obp)) 

anova(lm(bat10_test$obp~bat10_test$position))

#SSG, MSG
mu_total <-mean(bat10_test$obp)
bat10_data <-bat10_test %>% group_by(position) %>% summarize(n=n(),mu= mean(obp),sd=sd(obp))

ssg<-  sum((bat10_data$mu-mu_total)^2*bat10_data$n)
msg<-  ssg/(nrow(bat10_data)-1)

#SSE, MSE
sst <- sum((bat10_test$obp-mu_total)^2)
sse <- sst- ssg
mse <- sse/(nrow(bat10_test)-nrow(bat10_data))

#F
f <-msg/mse
df(f,nrow(bat10_data)-1,nrow(bat10_test)-nrow(bat10_data))

#test function
oneway.test(bat10_test$obp~bat10_test$position, var.equal = T)
summary(aov(bat10_test$obp~bat10_test$position))
anova(lm(bat10_test$obp~bat10_test$position))

curve(df(x, 3,572),0,10)

qt((1-0.90)/2, df=5, lower.tail = F)
pt(2.13,df=27,lower.tail = F)

qt(0.025, df=35)

pt(1.603,df=50, lower.tail = F)

qt(0.05,df=50)

pt(1.1/0.686,df=50,lower.tail = F)

pt(-2.71,df=5)*2
qt(0.05/2,df=5)

pt(3.016,df=9,lower.tail = F)*2

pt(2.24,df=21,lower.tail = F)

pt(1.889,df=13,lower.tail = F)
pt(1.348,df=13,lower.tail = F)

qnorm(0.975)


pt(-1.402,df=13, lower.tail = T)

pf(5.21,4,12210,lower.tail = F)

pt(2.54,df=39, lower.tail = F)*2
pnorm(2.892,lower.tail = F)*2

qt((1-0.95)/2,df=13)
qt(0.95,df=13,lower.tail = F)

pt(1.75,df=19,lower.tail=F)*2

pt(0.5,df=17,lower.tail = F)

pnorm(2.68,lower.tail = F)

qnorm(0.05)
qnorm(0.025)

qt(0.05,df=24)

pnorm(2.36,lower.tail = F)
pnorm(1.18, lower.tail = F)
pnorm(8.9, lower.tail = F)
pnorm(3.85,lower.tail = F)
pnorm(0.17,lower.tail = F)*2
pnorm(2.53,lower.tail = F)

x <-0:50
plot(x,dbinom(x,50,0.08))

x <-0:30
plot(x,dbinom(x,30,0.9))

pnorm(2.5,lower.tail = F)

curve(dbinom(x,50,0.08))

pnorm(20.45, lower.tail = F)

#Paul the octpus simulation

source("http://bit.ly/dasi_inference")
paul <- factor(c(rep("yes",8), rep("no",0)),levels=c("yes","no"))
inference(paul, est="proportion", type="ht", 
          method = "simulation",success = "yes", null=0.5,
          alternative = "greater")

#hand recognition

back_hand <- factor(c(rep("correct",11), rep("incorrect",1)),
               levels=c("correct","incorrect"))

palm_hand <- factor(c(rep("correct",7), rep("incorrect",5)),
                    levels=c("correct","incorrect"))

prop.test(c(11,7),c(12,12))


inference(hand, est="proportion", type="ht", 
          method = "simulation",success = "correct", null=0,
          alternative = "twosided",nsim=100)

?inference
data("atheism")
library("dplyr")
us12 <- atheism %>%
  filter(nationality == "United States" , atheism$year == "2012")
inference(y = response, data= us12, statistic= "proportion", type = "ci", method = "theoretical", success = "atheist")

unique(atheism$nationality)

curve(dchisq(x,df=1),0,10,col="red")
for(i in 2:3){
  curve(dchisq(x,df=i),0,10,add=T,)
}


curve(dchisq(x,df=2),0,10,col="red")

curve(dchisq(x,df=4),0,10,add=T, col="blue")

curve(dchisq(x,df=9),0,10,add=T, col="green")

legend("topright",legend=c("df=2","df=4","df=9"),lty=1,lwd=1,
       col=c("red","blue","green"))



pchisq(15.08,df=6,lower.tail = F)

no_research <-  c(3511,1749,1818)
new_research<- c(1489,751,682)

research <-data.frame(no_research, new_research)
research <- t(research)
chisq.test()

pchisq(106.37,df=2,lower.tail = F)

approve <- c(842,736,541)
disapprove <- c(616,646,842)

poll <- t(data.frame(approve, disapprove))
poll
chisq.test(poll)

test

chisq.test(test,df=2)


#bootstrap
outcomes <- c("complication", "no complication")

B <- 10000

sampling <- replicate(B,{
  p_sim <-sample(outcomes,62,prob=c(0.1,0.9),replace=T)
  mean(p_sim=="complication")
})

hist(sampling,breaks = seq(0,0.5,by=0.005))
abline(v=3/62,col="red")
p_value = mean(sampling<= 3/62)

#exact p_value

p_value = numeric()
for(i in 0:3 ){
  p = dbinom(i,62,0.1)
  p_value =append(p_value,p)
  
}
sum(p_value)

#goodness for fit

outcomes <-c("white","black","hispanic","other")
pro <- c(0.72,0.07,0.12,0.09)

B <-100000
chi_sq_dist <- replicate(B,{
  sample_pop <-sample(outcomes,100 ,prob=pro,replace=T)
  sample_pop <- factor(sample_pop,levels=outcomes)
  d <-data.frame(table(sample_pop))
  chi_sq <-(d$Freq[1]-pro[1]*100)^2/(pro[1]*100) +
    (d$Freq[2]-pro[2]*100)^2/(pro[2]*100) +
    (d$Freq[3]-pro[3]*100)^2/(pro[3]*100) +
    (d$Freq[4]-pro[4]*100)^2/(pro[4]*100)
})
max(chi_sq_dist)

hist(chi_sq_dist,breaks = seq(0,35,by=0.75))
abline(v=5.89,col="red")
mean(chi_sq_dist>=5.89)

pnorm(1.37,lower.tail = F)*2

#Simulating a difference under the null distribution

test_sample <- c(rep(0,65),rep(1,25))
test_sample
sample(test_sample, 40)


B=10000
p_dff_sampling <- replicate(B,{
  ctl <- sample(test_sample,50)  
  p_ctl <- mean(ctl)
  p_trt <- (25-sum(ctl))/40
  p_dff <- p_trt -p_ctl
 
})

hist(p_dff_sampling, breaks = seq(-0.4,0.4,by=0.8/17))
abline(v=0.13, col="red")

table(p_dff_sampling)
p_value =mean(p_dff_sampling>=p_dff_sampling[10])*2

d <-unique(p_dff_sampling)
sort(d)
s_data <-data.frame(table(p_dff_sampling))
(s_data$p_dff_sampling)>=0.13

ggplot(data=s_data) + 
  geom_bar(aes(x=p_dff_sampling,y=Freq),stat = "identity")+
  geom_vline(xintercept=0.13,col="red")


#another simulation
test_sample <- c(rep(0,25),rep(1,20))

B=10000
p_dff_sampling <- replicate(B,{
  ctl <- sample(test_sample,25)  
  p_ctl <- mean(ctl)
  p_trt <- (20-sum(ctl))/20
  p_dff <- p_trt -p_ctl
  
})

hist(p_dff_sampling, breaks = seq(-0.6,0.6,by=1.2/13))
table(p_dff_sampling)

table(p_dff_sampling)
p_value =mean(p_dff_sampling<=p_dff_sampling[21])*2

curve(dchisq(x,df=1000),0,10000)

pchisq(11.462,df=2, lower.tail = F)

chisq.test(matrix(c(154,180,104,132,126,131),nrow=3,ncol=2))

chisq.test(matrix(c(18,9,37,15,24,24),nrow=2,ncol=3))

p=0.5
n=10
B=1000
test_data <- replicate(B,{
  res=sample(c(1,0),prob=c(p,1-p),size=n,replace=T)
  r=sum(res)
  w=n-sum(res)
  c(r,w)
})
test_data <-data.frame(test_data)
test_data <- t(data.frame(test_data))              
              
test_data <-data.frame(test_data)
cor(test_data$X2, test_data$X1)

ggplot(data=test_data) + geom_point(aes(x=X1, y=X2))

