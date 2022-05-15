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


mu = mean(yrbss_sample_mean_1000$x_bar_physical)
s = sd(yrbss_sample_mean_1000$x_bar_physical)